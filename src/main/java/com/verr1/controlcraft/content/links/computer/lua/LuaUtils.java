package com.verr1.controlcraft.content.links.computer.lua;

import com.sun.management.ThreadMXBean;
import com.verr1.controlcraft.content.links.computer.lua.libs.ComputerWatcherLib;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.WatcherLib;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.DebugLib;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;

import java.lang.reflect.Array;
import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LuaUtils {

    public static final int DEFAULT_LOAD_MAX_INSTRUCTIONS = Integer.MAX_VALUE;
    public static final long DEFAULT_LOAD_MAX_TIME_MILLIS = 1000L;
    public static final long DEFAULT_LOAD_MAX_ALLOCATED_BYTES = Long.MAX_VALUE / 4L;

    private static final Pattern LUA_ERROR_CLASS_PREFIX =
            Pattern.compile("^org\\.luaj\\.vm2\\.LuaError:\\s*");
    private static final Pattern EMBEDDED_CHUNK_SOURCE =
            Pattern.compile("(?s)\\[string\\s+\".*?\"\\]");
    private static final Pattern LINE_AND_MESSAGE =
            Pattern.compile("(?m)(?:\\[script\\]|[^\\r\\n:]+):(\\d+):\\s*([^\\r\\n]+)");
    private static final ThreadMXBean THREAD_MX_BEAN;

    static {
        ThreadMXBean threadMxBean = null;
        try {
            threadMxBean = (ThreadMXBean) ManagementFactory.getThreadMXBean();
            if (threadMxBean.isThreadAllocatedMemorySupported()) {
                threadMxBean.setThreadAllocatedMemoryEnabled(true);
            }
        } catch (RuntimeException ignored) {
            threadMxBean = null;
        }
        THREAD_MX_BEAN = threadMxBean;
    }

    public static LuaTable makeTable(Map<String, Object> map) {
        LuaTable table = new LuaTable();
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            table.set(entry.getKey(), toLuaValue(entry.getValue()));
        }
        return table;
    }

    public static LuaValue toLuaValue(Object value) {
        if (value == null) {
            return LuaValue.NIL;
        }
        if (value instanceof LuaValue luaValue) {
            return luaValue;
        }
        if (value instanceof Boolean bool) {
            return LuaValue.valueOf(bool);
        }
        if (value instanceof Number number) {
            return LuaValue.valueOf(number.doubleValue());
        }
        if (value instanceof CharSequence text) {
            return LuaValue.valueOf(text.toString());
        }
        if (value instanceof Map<?, ?> map) {
            LuaTable table = new LuaTable();
            map.forEach((key, nestedValue) -> {
                if (key != null) {
                    table.set(String.valueOf(key), toLuaValue(nestedValue));
                }
            });
            return table;
        }
        if (value instanceof Collection<?> collection) {
            LuaTable table = new LuaTable();
            int index = 1;
            for (Object item : collection) {
                table.set(index++, toLuaValue(item));
            }
            return table;
        }
        if (value.getClass().isArray()) {
            LuaTable table = new LuaTable();
            int length = Array.getLength(value);
            for (int i = 0; i < length; i++) {
                table.set(i + 1, toLuaValue(Array.get(value, i)));
            }
            return table;
        }
        if (value.getClass().isEnum()) {
            return LuaValue.valueOf(((Enum<?>) value).name().toLowerCase());
        }
        return CoerceJavaToLua.coerce(value);
    }

    public static Object toJavaValue(LuaValue value) {
        return toJavaValue(value, 0, new IdentityHashMap<>());
    }

    private static Object toJavaValue(LuaValue value, int depth, IdentityHashMap<Object, Boolean> visited) {
        if (value == null || value.isnil()) {
            return null;
        }
        if (depth > 32) {
            return value.tojstring();
        }
        if (value.isboolean()) {
            return value.toboolean();
        }
        if (value.isnumber()) {
            return value.checkdouble();
        }
        if (value.isstring()) {
            return value.checkjstring();
        }
        if (value.istable()) {
            LuaTable table = value.checktable();
            if (visited.containsKey(table)) {
                return "<cycle>";
            }
            visited.put(table, Boolean.TRUE);
            try {
                return luaTableToJava(table, depth + 1, visited);
            } finally {
                visited.remove(table);
            }
        }
        if (value.isuserdata()) {
            Object userdata = value.touserdata();
            return userdata == null ? value.tojstring() : userdata;
        }
        return value.tojstring();
    }

    private static Object luaTableToJava(LuaTable table, int depth, IdentityHashMap<Object, Boolean> visited) {
        List<LuaValue> keys = collectLuaTableKeys(table);
        boolean contiguousArray = !keys.isEmpty();
        int arrayLength = 0;
        for (LuaValue key : keys) {
            int index = positiveIntegerKey(key);
            if (index <= 0) {
                contiguousArray = false;
                break;
            }
            arrayLength = Math.max(arrayLength, index);
        }

        if (contiguousArray) {
            for (int index = 1; index <= arrayLength; index++) {
                if (table.get(index).isnil()) {
                    contiguousArray = false;
                    break;
                }
            }
        }

        if (contiguousArray) {
            List<Object> list = new ArrayList<>(arrayLength);
            for (int index = 1; index <= arrayLength; index++) {
                list.add(toJavaValue(table.get(index), depth, visited));
            }
            return list;
        }

        Map<String, Object> map = new java.util.LinkedHashMap<>();
        for (LuaValue key : keys) {
            map.put(keyToJavaString(key), toJavaValue(table.get(key), depth, visited));
        }
        return map;
    }

    private static List<LuaValue> collectLuaTableKeys(LuaTable table) {
        List<LuaValue> keys = new ArrayList<>();
        LuaValue key = LuaValue.NIL;
        while (true) {
            Varargs next = table.next(key);
            key = next.arg1();
            if (key.isnil()) {
                break;
            }
            keys.add(key);
        }
        return keys;
    }

    private static int positiveIntegerKey(LuaValue value) {
        if (!value.isnumber()) {
            return -1;
        }
        double number = value.checkdouble();
        int integer = (int) number;
        if (integer <= 0 || number != integer) {
            return -1;
        }
        return integer;
    }

    private static String keyToJavaString(LuaValue key) {
        if (key.isstring()) {
            return key.checkjstring();
        }
        if (key.isnumber()) {
            double number = key.checkdouble();
            long integral = (long) number;
            if (integral == number) {
                return Long.toString(integral);
            }
            return Double.toString(number);
        }
        if (key.isboolean()) {
            return Boolean.toString(key.toboolean());
        }
        return key.tojstring();
    }

    public static LuaValue loadFunctionBlock(String code, String functionName, Globals luaGlobal)
            throws UndefineMethodException, LuaOvertimeException, CompletionException {
        return loadFunctionBlock(
                code,
                functionName,
                luaGlobal,
                DEFAULT_LOAD_MAX_INSTRUCTIONS,
                DEFAULT_LOAD_MAX_TIME_MILLIS,
                DEFAULT_LOAD_MAX_ALLOCATED_BYTES
        );
    }

    public static LuaValue loadFunctionBlock(
            String code,
            String functionName,
            Globals luaGlobal,
            int maxInstructions,
            long maxTimeMillis,
            long maxAllocatedBytes
    ) throws UndefineMethodException, LuaOvertimeException, CompletionException {
        long timeoutMillis = Math.max(1L, maxTimeMillis);

        CompletableFuture<LuaValue> future = CompletableFuture.supplyAsync(() -> {
                    LoadMonitor monitor = LoadMonitor.start(timeoutMillis, maxAllocatedBytes);
                    prepareLoadWatcher(luaGlobal, maxInstructions, timeoutMillis, maxAllocatedBytes);

                    LuaValue chunk = luaGlobal.load(code, "computer_" + functionName + ".lua");
                    monitor.checkNow("load");

                    chunk.call();
                    monitor.checkNow("initialization");

                    return luaGlobal.get(functionName);
                })
                .completeOnTimeout(LuaValue.NIL, timeoutMillis, TimeUnit.MILLISECONDS);

        LuaValue loopFunc;
        try {
            loopFunc = future.join();
            if (loopFunc == LuaValue.NIL) {
                if (future.isDone()) {
                    throw new UndefineMethodException(functionName + " is not present!");
                } else {
                    throw new LuaOvertimeException("Compiling Lua script overtime!");
                }
            }
        } catch (CompletionException e) {
            Throwable cause = e.getCause();
            if (cause instanceof LuaError luaError) {
                throw luaError;
            }
            if (cause instanceof LuaOvertimeException luaOvertimeException) {
                throw luaOvertimeException;
            }
            throw new RuntimeException(e);
        }

        return loopFunc;
    }

    public static Optional<LuaValue> safeLoadValue(String code, String functionName, Globals luaGlobal) {
        return safeLoadValue(
                code,
                functionName,
                luaGlobal,
                DEFAULT_LOAD_MAX_INSTRUCTIONS,
                DEFAULT_LOAD_MAX_TIME_MILLIS,
                DEFAULT_LOAD_MAX_ALLOCATED_BYTES
        );
    }

    public static Optional<LuaValue> safeLoadValue(
            String code,
            String functionName,
            Globals luaGlobal,
            int maxInstructions,
            long maxTimeMillis,
            long maxAllocatedBytes
    ) {
        try {
            return Optional.of(loadFunctionBlock(
                    code,
                    functionName,
                    luaGlobal,
                    maxInstructions,
                    maxTimeMillis,
                    maxAllocatedBytes
            ));
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    private static void prepareLoadWatcher(
            Globals luaGlobal,
            int maxInstructions,
            long maxTimeMillis,
            long maxAllocatedBytes
    ) {
        DebugLib debugLib = luaGlobal.debuglib;
        if (debugLib instanceof ComputerWatcherLib watcher) {
            watcher.setLimits(maxInstructions, maxTimeMillis, maxAllocatedBytes);
            watcher.resetForNewExecution();
            return;
        }
        if (debugLib instanceof WatcherLib watcher) {
            watcher.setLimits(maxInstructions, maxTimeMillis, maxAllocatedBytes);
            watcher.resetForNewExecution();
        }
    }

    public static String sanitizeLuaError(LuaError message) {
        return sanitizeLuaError(message.getMessage());
    }

    public static String sanitizeLuaError(LuaError message, int maxLen) {
        String err = sanitizeLuaError(message.getMessage());
        return err.substring(0, Math.min(err.length(), maxLen));
    }

    /**
     * Keep the misspelled name for compatibility with existing callers/user expectation.
     */
    public static String sanitizeLuaError(String message) {
        if (message == null || message.isBlank()) {
            return "Unknown Lua error";
        }

        String normalized = message
                .replace("\r\n", "\n")
                .replace('\r', '\n')
                .trim();
        normalized = LUA_ERROR_CLASS_PREFIX.matcher(normalized).replaceFirst("");

        // LuaJ may embed full chunk source in [string "..."], which makes the message huge.
        String compact = EMBEDDED_CHUNK_SOURCE.matcher(normalized).replaceAll("[script]");

        Matcher matcher = LINE_AND_MESSAGE.matcher(compact);
        if (matcher.find()) {
            return "line " + matcher.group(1) + ": " + matcher.group(2).trim();
        }

        int lineBreak = compact.indexOf('\n');
        String firstLine = lineBreak >= 0 ? compact.substring(0, lineBreak) : compact;
        return firstLine.trim();
    }

    private record LoadMonitor(long startTimeNanos, long maxTimeMillis, long startAllocatedBytes, long maxAllocatedBytes) {

        static LoadMonitor start(long maxTimeMillis, long maxAllocatedBytes) {
            long startAllocatedBytes = -1L;
            if (THREAD_MX_BEAN != null && THREAD_MX_BEAN.isThreadAllocatedMemoryEnabled()) {
                startAllocatedBytes = THREAD_MX_BEAN.getThreadAllocatedBytes(Thread.currentThread().getId());
            }
            return new LoadMonitor(System.nanoTime(), maxTimeMillis, startAllocatedBytes, maxAllocatedBytes);
        }

        void checkNow(String stage) {
            long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startTimeNanos);
            if (elapsedMillis > maxTimeMillis) {
                throw new LuaOvertimeException("Loading Lua script overtime during " + stage + "!");
            }

            if (THREAD_MX_BEAN != null && startAllocatedBytes >= 0L) {
                long allocatedDuringTask = THREAD_MX_BEAN.getThreadAllocatedBytes(Thread.currentThread().getId()) - startAllocatedBytes;
                if (allocatedDuringTask > maxAllocatedBytes) {
                    throw new LuaError(
                            String.format(
                                    "OOM Exception: Loading Lua script exceeded memory allocation limit (%.2f MB).",
                                    maxAllocatedBytes / 1024.0 / 1024.0
                            )
                    );
                }
            }
        }
    }
}

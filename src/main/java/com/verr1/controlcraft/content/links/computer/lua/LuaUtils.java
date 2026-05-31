package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.UndefineMethodException;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.jse.CoerceJavaToLua;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LuaUtils {

    private static final Pattern LUA_ERROR_CLASS_PREFIX =
            Pattern.compile("^org\\.luaj\\.vm2\\.LuaError:\\s*");
    private static final Pattern EMBEDDED_CHUNK_SOURCE =
            Pattern.compile("(?s)\\[string\\s+\".*?\"\\]");
    private static final Pattern LINE_AND_MESSAGE =
            Pattern.compile("(?m)(?:\\[script\\]|[^\\r\\n:]+):(\\d+):\\s*([^\\r\\n]+)");

    public static LuaTable makeTable(Map<String, Object> map) {
        LuaTable table = new LuaTable();
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            table.set(entry.getKey(), CoerceJavaToLua.coerce(entry.getValue()));
        }
        return table;
    }

    public static LuaValue loadFunctionBlock(String code, String functionName, Globals luaGlobal)
        throws UndefineMethodException, LuaOvertimeException, CompletionException
    {

        CompletableFuture<LuaValue> future = CompletableFuture.supplyAsync(() -> {
                LuaValue chunk = luaGlobal.load(code, "computer_" + functionName + ".lua");
                chunk.call();
                return luaGlobal.get(functionName);

            })
            .completeOnTimeout(LuaValue.NIL, 1000, TimeUnit.MILLISECONDS);

        LuaValue loopFunc;
        try{
            loopFunc = future.join();
            if(loopFunc == LuaValue.NIL){
                if(future.isDone()){
                    throw new UndefineMethodException(functionName + " is not present!");
                }else{
                    throw new LuaOvertimeException("Compiling Lua script overtime!");
                }
            }
        } catch (CompletionException e) {
            Throwable e0 = e.getCause();
            if(e0 instanceof LuaError luaError){
                throw luaError;
            }
            throw new RuntimeException(e);
        }

        return loopFunc;
    }

    public static Optional<LuaValue> safeLoadValue(String code, String functionName, Globals luaGlobal){
        try{
            return Optional.of(loadFunctionBlock(code, functionName, luaGlobal));
        } catch (Exception e) {
            return Optional.empty();
        }
    }


    public static String sanitizeLuaError(LuaError message){
        return sanitizeLuaError(message.getMessage());
    }

    public static String sanitizeLuaError(LuaError message, int maxLen){
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




}
package com.verr1.controlcraft.content.links.computer.lua.libs;

import org.joml.Quaterniondc;
import org.joml.Vector3dc;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

public class TextUtilLib extends TwoArgFunction {

    private static final Pattern IDENTIFIER = Pattern.compile("^[A-Za-z_][A-Za-z0-9_]*$");
    private static final int DEFAULT_MAX_DEPTH = 16;

    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaValue library = tableOf();

        library.set("serialize", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                LuaValue value = args.arg1();
                SerializeOptions options = SerializeOptions.from(args.arg(2));
                return LuaValue.valueOf(Serializer.serialize(value, options));
            }
        });

        env.set("TextUtil", library);
        return library;
    }

    private record SerializeOptions(
            boolean compact,
            boolean sortKeys,
            int maxDepth
    ) {
        private static SerializeOptions from(LuaValue arg) {
            if (arg == null || arg.isnil()) {
                return new SerializeOptions(true, true, DEFAULT_MAX_DEPTH);
            }

            if (arg.isboolean()) {
                return new SerializeOptions(arg.toboolean(), true, DEFAULT_MAX_DEPTH);
            }

            if (!arg.istable()) {
                return new SerializeOptions(true, true, DEFAULT_MAX_DEPTH);
            }

            LuaValue compactValue = arg.get("compact");
            LuaValue prettyValue = arg.get("pretty");
            LuaValue sortKeysValue = arg.get("sortKeys");
            LuaValue maxDepthValue = arg.get("maxDepth");

            boolean compact = compactValue.isnil() ? true : compactValue.toboolean();
            if (!prettyValue.isnil()) {
                compact = !prettyValue.toboolean();
            }

            boolean sortKeys = sortKeysValue.isnil() || sortKeysValue.toboolean();
            int maxDepth = maxDepthValue.isnil() ? DEFAULT_MAX_DEPTH : Math.max(1, maxDepthValue.checkint());
            return new SerializeOptions(compact, sortKeys, maxDepth);
        }
    }

    private static final class Serializer {

        private static final Comparator<LuaValue> KEY_COMPARATOR = (left, right) -> {
            int kindCompare = Integer.compare(keyKind(left), keyKind(right));
            if (kindCompare != 0) {
                return kindCompare;
            }

            return switch (keyKind(left)) {
                case 0 -> Double.compare(left.checkdouble(), right.checkdouble());
                case 1 -> left.checkjstring().compareTo(right.checkjstring());
                case 2 -> Boolean.compare(left.toboolean(), right.toboolean());
                default -> {
                    String leftText = left.typename() + ":" + left.tojstring();
                    String rightText = right.typename() + ":" + right.tojstring();
                    yield leftText.compareTo(rightText);
                }
            };
        };

        private final SerializeOptions options;
        private final IdentityHashMap<Object, Boolean> activeContainers = new IdentityHashMap<>();

        private Serializer(SerializeOptions options) {
            this.options = options;
        }

        private static String serialize(LuaValue value, SerializeOptions options) {
            return new Serializer(options).serializeLuaValue(value, 0);
        }

        private String serializeLuaValue(LuaValue value, int depth) {
            if (depth >= options.maxDepth()) {
                return quoteString("<max-depth>");
            }

            if (value == null || value.isnil()) {
                return "nil";
            }

            if (value.isboolean()) {
                return value.toboolean() ? "true" : "false";
            }

            if (value.isnumber()) {
                return serializeNumber(value.checkdouble(), value.tojstring());
            }

            if (value.isstring()) {
                return quoteString(value.checkjstring());
            }

            if (value.istable()) {
                return serializeLuaTable((LuaTable) value, depth);
            }

            if (value.isuserdata()) {
                return serializeJavaObject(value.touserdata(), depth);
            }

            if (value.isfunction()) {
                return quoteString("<function>");
            }

            if (value.isthread()) {
                return quoteString("<thread>");
            }

            return quoteString(value.tojstring());
        }

        private String serializeLuaTable(LuaTable table, int depth) {
            if (activeContainers.containsKey(table)) {
                return quoteString("<cycle>");
            }

            activeContainers.put(table, Boolean.TRUE);
            try {
                List<LuaValue> keys = collectKeys(table);
                Set<Integer> numericKeys = new HashSet<>();
                for (LuaValue key : keys) {
                    int arrayIndex = positiveIntegerKey(key);
                    if (arrayIndex > 0) {
                        numericKeys.add(arrayIndex);
                    }
                }

                int arrayLength = 0;
                while (numericKeys.contains(arrayLength + 1)) {
                    arrayLength++;
                }

                if (options.sortKeys()) {
                    keys.sort(KEY_COMPARATOR);
                }

                List<String> entries = new ArrayList<>();
                for (int i = 1; i <= arrayLength; i++) {
                    entries.add(serializeLuaValue(table.get(i), depth + 1));
                }

                for (LuaValue key : keys) {
                    int arrayIndex = positiveIntegerKey(key);
                    if (arrayIndex >= 1 && arrayIndex <= arrayLength) {
                        continue;
                    }

                    String keyText = serializeLuaKey(key, depth + 1);
                    String valueText = serializeLuaValue(table.get(key), depth + 1);
                    entries.add(keyText + " = " + valueText);
                }

                return formatEntries(entries, depth);
            } finally {
                activeContainers.remove(table);
            }
        }

        private String serializeJavaObject(Object object, int depth) {
            if (depth >= options.maxDepth()) {
                return quoteString("<max-depth>");
            }

            if (object == null) {
                return "nil";
            }

            if (object instanceof Boolean bool) {
                return bool ? "true" : "false";
            }

            if (object instanceof Byte || object instanceof Short || object instanceof Integer || object instanceof Long) {
                return object.toString();
            }

            if (object instanceof Float || object instanceof Double) {
                return serializeNumber(((Number) object).doubleValue(), object.toString());
            }

            if (object instanceof CharSequence || object instanceof Character || object instanceof Enum<?>) {
                return quoteString(String.valueOf(object));
            }

            if (object instanceof Vector3dc vector3d) {
                return formatEntries(List.of(
                        "x = " + serializeNumber(vector3d.x(), Double.toString(vector3d.x())),
                        "y = " + serializeNumber(vector3d.y(), Double.toString(vector3d.y())),
                        "z = " + serializeNumber(vector3d.z(), Double.toString(vector3d.z()))
                ), depth);
            }

            if (object instanceof Quaterniondc quaternion) {
                return formatEntries(List.of(
                        "x = " + serializeNumber(quaternion.x(), Double.toString(quaternion.x())),
                        "y = " + serializeNumber(quaternion.y(), Double.toString(quaternion.y())),
                        "z = " + serializeNumber(quaternion.z(), Double.toString(quaternion.z())),
                        "w = " + serializeNumber(quaternion.w(), Double.toString(quaternion.w()))
                ), depth);
            }

            if (object instanceof Map<?, ?> map) {
                return serializeJavaMap(map, depth);
            }

            if (object instanceof Iterable<?> iterable) {
                return serializeJavaIterable(iterable, depth);
            }

            if (object.getClass().isArray()) {
                return serializeJavaArray(object, depth);
            }

            return quoteString(String.valueOf(object));
        }

        private String serializeJavaMap(Map<?, ?> map, int depth) {
            if (activeContainers.containsKey(map)) {
                return quoteString("<cycle>");
            }

            activeContainers.put(map, Boolean.TRUE);
            try {
                List<Map.Entry<?, ?>> entries = new ArrayList<>(map.entrySet());
                if (options.sortKeys()) {
                    entries.sort(Comparator.comparing(entry -> String.valueOf(entry.getKey())));
                }

                List<String> serializedEntries = new ArrayList<>();
                for (Map.Entry<?, ?> entry : entries) {
                    String keyText = serializeJavaKey(entry.getKey(), depth + 1);
                    String valueText = serializeJavaPojo(entry.getValue(), depth + 1);
                    serializedEntries.add(keyText + " = " + valueText);
                }
                return formatEntries(serializedEntries, depth);
            } finally {
                activeContainers.remove(map);
            }
        }

        private String serializeJavaIterable(Iterable<?> iterable, int depth) {
            if (activeContainers.containsKey(iterable)) {
                return quoteString("<cycle>");
            }

            activeContainers.put(iterable, Boolean.TRUE);
            try {
                List<String> serializedEntries = new ArrayList<>();
                for (Object value : iterable) {
                    serializedEntries.add(serializeJavaPojo(value, depth + 1));
                }
                return formatEntries(serializedEntries, depth);
            } finally {
                activeContainers.remove(iterable);
            }
        }

        private String serializeJavaArray(Object array, int depth) {
            if (activeContainers.containsKey(array)) {
                return quoteString("<cycle>");
            }

            activeContainers.put(array, Boolean.TRUE);
            try {
                int length = Array.getLength(array);
                List<String> serializedEntries = new ArrayList<>(length);
                for (int i = 0; i < length; i++) {
                    serializedEntries.add(serializeJavaPojo(Array.get(array, i), depth + 1));
                }
                return formatEntries(serializedEntries, depth);
            } finally {
                activeContainers.remove(array);
            }
        }

        private String serializeJavaPojo(Object object, int depth) {
            if (object instanceof LuaValue luaValue) {
                return serializeLuaValue(luaValue, depth);
            }
            return serializeJavaObject(object, depth);
        }

        private String serializeLuaKey(LuaValue key, int depth) {
            if (key.isstring()) {
                String text = key.checkjstring();
                if (IDENTIFIER.matcher(text).matches()) {
                    return text;
                }
            }
            return "[" + serializeLuaValue(key, depth) + "]";
        }

        private String serializeJavaKey(Object key, int depth) {
            if (key instanceof String stringKey && IDENTIFIER.matcher(stringKey).matches()) {
                return stringKey;
            }
            if (key instanceof LuaValue luaValue) {
                return "[" + serializeLuaValue(luaValue, depth) + "]";
            }
            return "[" + serializeJavaPojo(key, depth) + "]";
        }

        private String formatEntries(List<String> entries, int depth) {
            if (entries.isEmpty()) {
                return "{}";
            }

            if (options.compact()) {
                return "{ " + String.join(", ", entries) + " }";
            }

            String indent = "  ".repeat(depth);
            String childIndent = "  ".repeat(depth + 1);
            return "{\n"
                    + childIndent
                    + String.join(",\n" + childIndent, entries)
                    + "\n"
                    + indent
                    + "}";
        }

        private static List<LuaValue> collectKeys(LuaTable table) {
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

        private static int keyKind(LuaValue value) {
            if (value.isnumber()) {
                return 0;
            }
            if (value.isstring()) {
                return 1;
            }
            if (value.isboolean()) {
                return 2;
            }
            return 3;
        }

        private static String serializeNumber(double number, String fallback) {
            if (Double.isNaN(number)) {
                return "0/0";
            }
            if (Double.isInfinite(number)) {
                return number > 0 ? "math.huge" : "-math.huge";
            }
            return fallback;
        }

        private static String quoteString(String text) {
            StringBuilder builder = new StringBuilder(text.length() + 2);
            builder.append('"');
            for (int i = 0; i < text.length(); i++) {
                char c = text.charAt(i);
                switch (c) {
                    case '\\' -> builder.append("\\\\");
                    case '"' -> builder.append("\\\"");
                    case '\n' -> builder.append("\\n");
                    case '\r' -> builder.append("\\r");
                    case '\t' -> builder.append("\\t");
                    case '\b' -> builder.append("\\b");
                    case '\f' -> builder.append("\\f");
                    default -> {
                        if (c < 32) {
                            builder.append(String.format("\\u%04x", (int) c));
                        } else {
                            builder.append(c);
                        }
                    }
                }
            }
            builder.append('"');
            return builder.toString();
        }
    }
}
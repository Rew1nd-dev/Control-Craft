package com.verr1.controlcraft.content.links.computer.lua;

import net.minecraft.nbt.*;
import java.util.List;
import java.util.Map;

public class LuaNbtSerializer {

    /**
     * 将 Java Object (从 Lua 转换而来) 序列化为 Minecraft NBT Tag
     */
    public static Tag serialize(Object obj) {
        if (obj == null) {
            return EndTag.INSTANCE; // 相当于 null/nil
        }
        
        // 1. 基础类型映射
        if (obj instanceof String str) {
            return StringTag.valueOf(str);
        } else if (obj instanceof Double d) {
            return DoubleTag.valueOf(d);
        } else if (obj instanceof Integer i) {
            return IntTag.valueOf(i);
        } else if (obj instanceof Boolean b) {
            return ByteTag.valueOf(b ? (byte) 1 : (byte) 0); // NBT 用 Byte(0/1) 存 Boolean
        } else if (obj instanceof Float f) {
            return FloatTag.valueOf(f);
        } else if (obj instanceof Long l) {
            return LongTag.valueOf(l);
        } else if (obj instanceof Byte || obj instanceof Short) {
            return IntTag.valueOf(((Number) obj).intValue()); 
        }

        // 2. 复合类型映射 - Map (对应 Lua 中的 Table - 字典形式)
        if (obj instanceof Map<?, ?> map) {
            CompoundTag compound = new CompoundTag();
            for (Map.Entry<?, ?> entry : map.entrySet()) {
                String key = String.valueOf(entry.getKey());
                Tag valueTag = serialize(entry.getValue());
                if (valueTag != EndTag.INSTANCE) { // 不保存 null 值
                    compound.put(key, valueTag);
                }
            }
            return compound;
        }

        // 3. 列表类型映射 - List (对应 Lua 中的 Table - 数组形式)
        if (obj instanceof List<?> list) {
            ListTag listTag = new ListTag();
            for (Object item : list) {
                Tag itemTag = serialize(item);
                if (itemTag != EndTag.INSTANCE) {
                    listTag.add(itemTag);
                }
            }
            return listTag;
        }

        // 如果是不支持的高级类型（如线程、闭包等），为了安全起见将其转为 String 或丢弃
        return StringTag.valueOf(obj.toString());
    }

    /**
     * 将 Minecraft NBT Tag 反序列化为 Java Object (供客户端恢复并交回给 Lua)
     */
    public static Object deserialize(Tag tag) {
        if (tag == null || tag instanceof EndTag) {
            return null;
        }

        if (tag instanceof StringTag strTag) {
            return strTag.getAsString();
        } else if (tag instanceof NumericTag numTag) {
            // 根据类型的不同精确还原
            if (tag instanceof DoubleTag) return numTag.getAsDouble();
            if (tag instanceof IntTag) return numTag.getAsInt();
            if (tag instanceof ByteTag) return numTag.getAsByte() != 0; // 恢复 Boolean
            if (tag instanceof FloatTag) return numTag.getAsFloat();
            if (tag instanceof LongTag) return numTag.getAsLong();
            return numTag.getAsDouble(); // fallback
        } else if (tag instanceof CompoundTag compound) {
            Map<String, Object> map = new java.util.HashMap<>();
            for (String key : compound.getAllKeys()) {
                map.put(key, deserialize(compound.get(key)));
            }
            return map;
        } else if (tag instanceof ListTag list) {
            List<Object> javaList = new java.util.ArrayList<>();
            for (Tag item : list) {
                javaList.add(deserialize(item));
            }
            return javaList;
        }

        return tag.getAsString();
    }
}

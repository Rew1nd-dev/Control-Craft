package com.verr1.controlcraft.registry.script;

import com.google.gson.*;
import com.google.gson.stream.JsonWriter;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.TreeMap;

public class JsonKeySynchronizer {

    public static void main(String[] args) {

        String zhCnPath = System.getProperty("user.dir") + "\\src\\main\\resources\\assets\\vscontrolcraft\\lang\\zh_cn.json";
        String enUsPath = System.getProperty("user.dir") + "\\src\\generated\\resources\\assets\\vscontrolcraft\\lang\\en_us.json";

        try {
            // 读取JSON文件
            JsonObject zhCnJson = readJsonFile(zhCnPath);
            JsonObject enUsJson = readJsonFile(enUsPath);

            // 同步键值
            JsonObject updatedZhCn = synchronizeKeys(zhCnJson, enUsJson);

            // 排序键
            JsonObject sortedJson = sortJsonKeys(updatedZhCn);

            // 写回文件（保留原始格式）
            writeJsonFile(zhCnPath, sortedJson);
            System.out.println("同步完成！更新后的文件: " + zhCnPath);

        } catch (IOException e) {
            System.err.println("文件错误: " + e.getMessage());
        } catch (JsonSyntaxException e) {
            System.err.println("JSON解析错误: " + e.getMessage());
        }
    }

    private static JsonObject readJsonFile(String path) throws IOException {
        String content = new String(Files.readAllBytes(Paths.get(path)));
        return JsonParser.parseString(content).getAsJsonObject();
    }

    private static JsonObject synchronizeKeys(JsonObject zhCn, JsonObject enUs) {
        JsonObject result = zhCn.deepCopy();

        for (Map.Entry<String, JsonElement> entry : enUs.entrySet()) {
            String key = entry.getKey();
            JsonElement value = entry.getValue();

            if (!zhCn.has(key)) {
                result.add(key, value);
                System.out.println("添加缺失键: " + key);
            }
        }
        return result;
    }

    private static JsonObject sortJsonKeys(JsonObject json) {
        TreeMap<String, JsonElement> sortedMap = new TreeMap<>();

        for (Map.Entry<String, JsonElement> entry : json.entrySet()) {
            sortedMap.put(entry.getKey(), entry.getValue());
        }

        JsonObject sortedJson = new JsonObject();
        sortedMap.forEach(sortedJson::add);
        return sortedJson;
    }

    private static void writeJsonFile(String path, JsonObject json) throws IOException {
        Gson gson = new GsonBuilder()
                .setPrettyPrinting()
                .disableHtmlEscaping()
                .create();

        try (FileWriter writer = new FileWriter(path)) {
            gson.toJson(json, writer);
        }
    }
}

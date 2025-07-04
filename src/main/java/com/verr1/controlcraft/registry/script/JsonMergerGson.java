package com.verr1.controlcraft.registry.script;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;

public class JsonMergerGson {
    private static final Gson gson = new GsonBuilder().setPrettyPrinting().disableHtmlEscaping().create();

    public static void main(String[] args) {


        String inputDirPath = System.getProperty("user.dir") + "\\src\\main\\resources\\assets\\vscontrolcraft\\lang\\ponder\\en_us";;
        String outputFilePath = System.getProperty("user.dir") + "\\src\\main\\resources\\assets\\vscontrolcraft\\lang\\ponder\\all.json";;

        try {
            mergeJsonFiles(inputDirPath, outputFilePath);
            System.out.println("JSON files merged successfully!");
            System.out.println("Output: " + new File(outputFilePath).getAbsolutePath());
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    public static void mergeJsonFiles(String inputDirPath, String outputFilePath) throws IOException {
        Path inputDir = Paths.get(inputDirPath);

        // 验证输入目录
        if (!Files.isDirectory(inputDir)) {
            throw new IOException("Input path is not a directory: " + inputDirPath);
        }

        // 创建输出目录（如果不存在）
        Path outputPath = Paths.get(outputFilePath);
        if (outputPath.getParent() != null) {
            Files.createDirectories(outputPath.getParent());
        }

        // 创建合并后的JSON对象
        JsonObject mergedJson = new JsonObject();

        // 获取所有JSON文件并按文件名排序
        Files.walk(inputDir)
                .filter(Files::isRegularFile)
                .filter(path -> path.toString().toLowerCase().endsWith(".json"))
                .sorted(Comparator.comparing(Path::getFileName))
                .forEach(path -> {
                    try {
                        // 读取JSON文件内容
                        String content = Files.readString(path);

                        // 解析JSON对象
                        JsonObject json = JsonParser.parseString(content).getAsJsonObject();

                        // 合并到结果对象
                        json.entrySet().forEach(entry -> {
                            mergedJson.add(entry.getKey(), entry.getValue());
                        });

                        System.out.println("Processed: " + path.getFileName());
                    } catch (Exception e) {
                        System.err.println("Error processing " + path + ": " + e.getMessage());
                    }
                });

        // 检查是否有文件被处理
        if (mergedJson.size() == 0) {
            throw new IOException("No valid JSON files found in directory: " + inputDirPath);
        }

        // 将合并后的JSON写入输出文件
        Files.writeString(outputPath, gson.toJson(mergedJson));
    }
}
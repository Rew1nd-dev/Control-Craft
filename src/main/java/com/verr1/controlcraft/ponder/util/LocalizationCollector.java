package com.verr1.controlcraft.ponder.util;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import net.minecraft.network.chat.Component;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class LocalizationCollector {
    public static boolean ENABLED = false;
    public static final String OUTPUT_PATH_PREFIX = System.getProperty("user.dir") + "\\..\\src\\main\\resources\\assets\\vscontrolcraft\\lang\\ponder\\en_us\\";
    private boolean closed = false;
    List<String> collected = new ArrayList<>();
    int calls = 0;
    final String title;

    public LocalizationCollector(String title) {
        this.title = title;
    }

    public String collect(String newText){
        calls++;
        if (ENABLED){
            collected.add(newText); // calls 0 -> collected.get(0)
            return newText;
        }else {
            return Component.translatable(key(calls - 1, title)).getString();
        }
    }

    public void end(){
        if(!ENABLED)return;
        if(closed)throw new IllegalStateException("LocalizationCollector is already closed for " + title);
        closed = true;
        try{
            writeJsonFile(OUTPUT_PATH_PREFIX + title + ".json"); // title
        }catch (IOException e) {
            throw new RuntimeException("Failed to write localization file", e);
        }
    }

    private static String key(int calls, String title){
        return "vscontrolcraft.ponder." + title + ".text." + calls;
    }

    private void writeJsonFile(String path) throws IOException {
        Gson gson = new GsonBuilder()
                .setPrettyPrinting()
                .disableHtmlEscaping()
                .create();

        JsonObject jsonObject = new JsonObject();
        for (int i = 0; i < collected.size(); i++) {
            jsonObject.addProperty(key(i, title), collected.get(i));
        }

        try {
            Path parentDir = Paths.get(path).getParent();
            if (parentDir != null) {
                Files.createDirectories(parentDir);
            }
        } catch (IOException e) {
            throw new RuntimeException("Failed to create directories for path: " + path, e);
        }

        try (FileWriter writer = new FileWriter(path)) {
            gson.toJson(jsonObject, writer);
        }
    }
}

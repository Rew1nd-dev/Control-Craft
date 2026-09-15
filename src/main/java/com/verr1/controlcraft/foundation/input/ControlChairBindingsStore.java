package com.verr1.controlcraft.foundation.input;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.verr1.controlcraft.ControlCraft;
import net.minecraftforge.fml.loading.FMLPaths;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public final class ControlChairBindingsStore {
    private static final Gson GSON = new GsonBuilder()
            .setPrettyPrinting()
            .disableHtmlEscaping()
            .create();
    private static final Path FILE = FMLPaths.CONFIGDIR.get()
            .resolve("controlcraft")
            .resolve("control_chair_bindings.json");

    private static volatile ControlChairBindingsProfile current = ControlChairBindingsProfile.defaults();

    private ControlChairBindingsStore() {
    }

    public static void load() {
        if (!Files.exists(FILE)) {
            current = ControlChairBindingsProfile.defaults();
            save();
            return;
        }

        try {
            String content = Files.readString(FILE);
            JsonObject json = JsonParser.parseString(content).getAsJsonObject();
            current = ControlChairBindingsProfile.fromJson(json);
        } catch (Exception e) {
            ControlCraft.LOGGER.warn("Failed to load control chair bindings, resetting to defaults", e);
            current = ControlChairBindingsProfile.defaults();
            save();
        }
    }

    public static void save() {
        try {
            Files.createDirectories(FILE.getParent());
            Files.writeString(FILE, GSON.toJson(current.toJson()));
        } catch (IOException e) {
            ControlCraft.LOGGER.warn("Failed to save control chair bindings", e);
        }
    }

    public static ControlChairBindingsProfile current() {
        return current;
    }

    public static void replace(ControlChairBindingsProfile profile) {
        current = profile.copy();
        save();
    }

    public static void resetToDefaults() {
        current = ControlChairBindingsProfile.defaults();
        save();
    }
}

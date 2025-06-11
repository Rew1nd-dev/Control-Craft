package com.verr1.controlcraft.registry;

import com.mojang.blaze3d.platform.InputConstants;
import com.verr1.controlcraft.ControlCraft;
import net.minecraft.client.KeyMapping;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.common.util.Lazy;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import org.lwjgl.glfw.GLFW;

@Mod.EventBusSubscriber(value = Dist.CLIENT, modid = ControlCraft.MODID, bus = Mod.EventBusSubscriber.Bus.MOD)
public class ControlCraftAllKeyBindings {

    public static final Lazy<KeyMapping> LINK_LAST_CAMERA = Lazy.of(() -> new KeyMapping(
            "key.controlcraft.link", // Will be localized using this translation key
            InputConstants.Type.KEYSYM, // Default mapping is on the keyboard
            GLFW.GLFW_KEY_P, // Default key is P
            "key.categories.controlcraft" // Mapping will be in the misc category
    ));

    @SubscribeEvent
    public void registerBindings(RegisterKeyMappingsEvent event) {
        event.register(LINK_LAST_CAMERA.get());
    }

}

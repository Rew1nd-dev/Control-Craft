package com.verr1.controlcraft.registry;

import com.mojang.blaze3d.platform.InputConstants;
import com.verr1.controlcraft.ControlCraft;
import net.minecraft.client.KeyMapping;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.event.RegisterKeyMappingsEvent;
import net.minecraftforge.common.util.Lazy;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.common.Mod;
import org.lwjgl.glfw.GLFW;

@Mod.EventBusSubscriber(value = Dist.CLIENT, bus = Mod.EventBusSubscriber.Bus.MOD)
public class ControlCraftAllKeyBindings {

    public static final Lazy<KeyMapping> LINK_LAST_CAMERA = Lazy.of(() -> new KeyMapping(
            "key.controlcraft.link", // Will be localized using this translation key
            InputConstants.Type.KEYSYM, // Default mapping is on the keyboard
            GLFW.GLFW_KEY_P, // Default key is P
            "Control Craft" // Mapping will be in the misc category
    ));

    public static final Lazy<KeyMapping> TOGGLE_CONTROL_CHAIR_INPUT = Lazy.of(() -> new KeyMapping(
            "key.controlcraft.toggle_control_chair_input",
            InputConstants.Type.KEYSYM,
            GLFW.GLFW_KEY_R,
            "Control Craft"
    ));

    public static final Lazy<KeyMapping> TOGGLE_CONTROL_CHAIR_VIEW_LOCK = Lazy.of(() -> new KeyMapping(
            "key.controlcraft.toggle_control_chair_view_lock",
            InputConstants.Type.KEYSYM,
            GLFW.GLFW_KEY_V,
            "Control Craft"
    ));

    public static final Lazy<KeyMapping> RESET_CONTROL_CHAIR_VIEW = Lazy.of(() -> new KeyMapping(
            "key.controlcraft.reset_control_chair_view",
            InputConstants.Type.KEYSYM,
            GLFW.GLFW_KEY_B,
            "Control Craft"
    ));

    public static final Lazy<KeyMapping> OPEN_CONTROL_CHAIR_BINDINGS = Lazy.of(() -> new KeyMapping(
            "key.controlcraft.open_control_chair_bindings",
            InputConstants.Type.KEYSYM,
            GLFW.GLFW_KEY_G,
            "Control Craft"
    ));

    @SubscribeEvent
    public static void registerBindings(RegisterKeyMappingsEvent event) {
        event.register(LINK_LAST_CAMERA.get());
        event.register(TOGGLE_CONTROL_CHAIR_INPUT.get());
        event.register(TOGGLE_CONTROL_CHAIR_VIEW_LOCK.get());
        event.register(RESET_CONTROL_CHAIR_VIEW.get());
        event.register(OPEN_CONTROL_CHAIR_BINDINGS.get());
    }

}

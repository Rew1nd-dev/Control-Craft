package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.AssetLookup;
import com.tterrag.registrate.util.entry.ItemEntry;
import com.verr1.controlcraft.content.items.AweInWandItem;
import com.verr1.controlcraft.content.items.CameraLinkItem;
import com.verr1.controlcraft.content.items.CircuitCompilerItem;
import com.verr1.controlcraft.content.items.CommonManualItem;

import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class ControlCraftItems {

    static {
        REGISTRATE.setCreativeTab(ControlCraftCreativeTabs.MAIN);
    }

    public static final ItemEntry<AweInWandItem> ALL_IN_WAND = REGISTRATE.item("awe_in_wand", AweInWandItem::new)
            .model(AssetLookup.existingItemModel())
            .properties(p -> p.stacksTo(1))
            .lang("Awe-In-Wand")
            .register();

    public static final ItemEntry<CameraLinkItem> CAMERA_LINK = REGISTRATE.item("camera_link", CameraLinkItem::new)
            .model(AssetLookup.existingItemModel())
            .properties(p -> p.stacksTo(1))
            .lang("Camera Link")
            .register();


    static {
        REGISTRATE.setCreativeTab(ControlCraftCreativeTabs.CIMULINK);
    }

    public static final ItemEntry<CircuitCompilerItem> CIRCUIT_COMPILER = REGISTRATE.item("compiler", CircuitCompilerItem::new)
            .model(AssetLookup.existingItemModel())
            .properties(p -> p.stacksTo(1))
            .lang("Circuit Compiler")
            .register();

    public static void register(){

    }
}

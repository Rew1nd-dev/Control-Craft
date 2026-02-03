package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.AssetLookup;
import com.tterrag.registrate.util.entry.ItemEntry;
import com.verr1.controlcraft.content.items.*;

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

    public static final ItemEntry<LuaCompilerItem> LUA_COMPILER = REGISTRATE.item("lua_compiler", LuaCompilerItem::new)
            .model(AssetLookup.existingItemModel())
            .properties(p -> p.stacksTo(1))
            .lang("Lua Compiler")
            .register();

    public static final ItemEntry<CimulinkRecordItem> LINK_RECORD = REGISTRATE.item("link_record", CimulinkRecordItem::new)
            //.model(AssetLookup.existingItemModel())
            .properties(p -> p.stacksTo(1))
            .lang("Link Record")
            .register();

    public static void register(){

    }
}

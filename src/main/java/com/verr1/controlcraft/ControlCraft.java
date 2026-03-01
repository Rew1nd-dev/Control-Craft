package com.verr1.controlcraft;

import com.mojang.logging.LogUtils;
import com.simibubi.create.foundation.data.CreateRegistrate;
import com.verr1.controlcraft.config.BlockPropertyConfig;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.content.compact.shaolib.ShaoLibCompact;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerCompact;
import com.verr1.controlcraft.content.compact.vssw.VSSWCompact;
import com.verr1.controlcraft.foundation.cimulink.core.components.lua.LuaScriptLoader;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.ponder.CimulinkPonderIndex;
import com.verr1.controlcraft.registry.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.AddReloadListenerEvent;
import net.minecraftforge.event.BuildCreativeModeTabContentsEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.fml.loading.FMLEnvironment;
import org.joml.Random;
import org.slf4j.Logger;


@Mod(ControlCraft.MODID)
@SuppressWarnings("removal")
public class ControlCraft
{

    public static final String MODID = "vscontrolcraft";
    public static final Random RANDOM_GENERATOR = new Random();
    public static final Logger LOGGER = LogUtils.getLogger();
    public static final CreateRegistrate REGISTRATE = CreateRegistrate.create(ControlCraft.MODID);

    public ControlCraft(){
        IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
        REGISTRATE.registerEventListeners(modEventBus);

        // Register the commonSetup method for modloading
        modEventBus.addListener(this::commonSetup);

        ControlCraftCreativeTabs.register(modEventBus);
        // ControlCraftManuals.register(modEventBus);

        ControlCraftBlocks.register();
        ControlCraftEntities.register();
        ControlCraftBlockEntities.register();
        CimulinkBlocks.register();
        CimulinkBlockEntities.register();
        ControlCraftPackets.registerPackets();
        ControlCraftItems.register();
        ControlCraftMenuTypes.register();
        ControlCraftDataGen.registerEnumDescriptions();
        CimulinkFactory.register();
        // CimulinkPonderIndex.register();



        TweakControllerCompact.init();
        CreateBigCannonsCompact.init();
        ShaoLibCompact.init();
        VSSWCompact.init();
        modEventBus.addListener(EventPriority.LOWEST, ControlCraftDataGen::gatherData);

        // modEventBus.addListener((e) -> ControlCraftAttachments.register());

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ControlCraftClient::clientInit);
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ControlCraftServer::ServerInit);
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CimulinkPonderIndex::register);

        MinecraftForge.EVENT_BUS.register(this);
        // Register our mod's ForgeConfigSpec so that Forge can create and load the config file for us
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, BlockPropertyConfig.SPEC);
    }



    public ControlCraft(FMLJavaModLoadingContext context)
    {
        IEventBus modEventBus = context.getModEventBus();

        REGISTRATE.registerEventListeners(modEventBus);
        modEventBus.addListener(this::commonSetup);


        ControlCraftCreativeTabs.register(modEventBus);

        ControlCraftBlocks.register();
        ControlCraftEntities.register();
        ControlCraftBlockEntities.register();
        CimulinkBlocks.register();
        CimulinkBlockEntities.register();
        ControlCraftPackets.registerPackets();
        ControlCraftItems.register();
        ControlCraftMenuTypes.register();
        ControlCraftDataGen.registerEnumDescriptions();
        CimulinkFactory.register();
        // CimulinkPonderIndex.register();
        // ControlCraftAttachments.register();
        TweakControllerCompact.init();
        CreateBigCannonsCompact.init();
        ShaoLibCompact.init();
        VSSWCompact.init();
        // AttachmentRegistry.register();

        // modEventBus.addListener((e) -> ControlCraftAttachments.register());
        modEventBus.addListener(EventPriority.LOWEST, ControlCraftDataGen::gatherData);
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ControlCraftClient::clientInit);
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ControlCraftServer::ServerInit);
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CimulinkPonderIndex::register);


        MinecraftForge.EVENT_BUS.register(this);

        context.registerConfig(ModConfig.Type.COMMON, BlockPropertyConfig.SPEC);


    }

    private void config(ModLoadingContext context){


        // context.registerConfig(ModConfig.Type.COMMON, BlockPropertyConfig.SPEC);
        // context.registerConfig(ModConfig.Type.COMMON, PermissionConfig.SPEC);
    }


    private void commonSetup(final FMLCommonSetupEvent event)
    {
        // Some common setup code
        LOGGER.info("HELLO FROM COMMON SETUP");

        if (BlockPropertyConfig._CC_OVERCLOCKING) LOGGER.info("CC OverClocked");


    }

    @SubscribeEvent
    public void onAddReloadListeners(AddReloadListenerEvent event) {
        event.addListener(new LuaScriptLoader());
    }


    @SubscribeEvent
    public void onServerStarting(ServerStartingEvent event)
    {
        LOGGER.info("HELLO from server starting");
    }


    public static ResourceLocation asResource(String path) {
        return new ResourceLocation(MODID, path);
    }
}

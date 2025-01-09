package com.verr1.vscontrolcraft;

import com.mojang.logging.LogUtils;
import com.simibubi.create.foundation.data.CreateRegistrate;
import com.verr1.vscontrolcraft.registry.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.BuildCreativeModeTabContentsEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
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
import org.slf4j.Logger;

// The value here should match an entry in the META-INF/mods.toml file
@Mod(ControlCraft.MODID)
public class ControlCraft
{
    // Define mod id in a common place for everything to reference
    public static final String MODID = "vscontrolcraft";
    // Directly reference a slf4j logger
    public static final Logger LOGGER = LogUtils.getLogger();
    public static final CreateRegistrate REGISTRATE = CreateRegistrate.create(ControlCraft.MODID);
    // Creates a creative tab with the id "examplemod:example_tab" for the example item, that is placed after the combat tab
    public ControlCraft(){
        IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
        REGISTRATE.registerEventListeners(modEventBus);
        modEventBus.addListener(this::addCreative);
        // Register the commonSetup method for modloading
        modEventBus.addListener(this::commonSetup);

        if(FMLEnvironment.dist == Dist.CLIENT){
            modEventBus.addListener(this::clientSetup);
        }
        modEventBus.addListener(this::addCreative);

        AllCreativeTabs.register(modEventBus);
        AllBlocks.register();
        AllBlockEntities.register();
        AllItems.register();
        AllPackets.registerPackets();

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
            ControlCraftClient.clientInit();
        });

        MinecraftForge.EVENT_BUS.register(this);
        ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.SPEC);// Register our mod's ForgeConfigSpec so that Forge can create and load the config file for us
    }



    public ControlCraft(FMLJavaModLoadingContext context)
    {
        IEventBus modEventBus = context.getModEventBus();

        REGISTRATE.registerEventListeners(modEventBus);
        modEventBus.addListener(this::addCreative);
        // Register the commonSetup method for modloading
        modEventBus.addListener(this::commonSetup);

        if(FMLEnvironment.dist == Dist.CLIENT){
            modEventBus.addListener(this::clientSetup);
        }

        // Register ourselves for server and other game events we are interested in


        // Register the item to a creative tab
        modEventBus.addListener(this::addCreative);

        AllCreativeTabs.register(modEventBus);
        AllBlocks.register();
        AllBlockEntities.register();
        AllItems.register();
        AllPackets.registerPackets();

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> ControlCraftClient::clientInit);

        MinecraftForge.EVENT_BUS.register(this);
        context.registerConfig(ModConfig.Type.COMMON, Config.SPEC);

    }

    private void commonSetup(final FMLCommonSetupEvent event)
    {
        // Some common setup code
        LOGGER.info("HELLO FROM COMMON SETUP");

        if (Config.OverclockComputerCraft) LOGGER.info("CC OverClocked");


    }

    private void clientSetup(FMLClientSetupEvent event)
    {


    }


    private void addCreative(BuildCreativeModeTabContentsEvent event)
    {

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

package com.verr1.controlcraft;

import com.mojang.logging.LogUtils;
import com.simibubi.create.foundation.data.CreateRegistrate;
import com.verr1.controlcraft.config.BlockPropertyConfig;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerCompact;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.ponder.CimulinkPonderIndex;
import com.verr1.controlcraft.registry.*;
import net.minecraft.resources.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
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
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.config.Configurator;
import org.joml.Random;
import org.slf4j.Logger;


/*
* TODO:
*    Visualization:
*    1.  add tool tips for blocks, need data syncing.
*    2.√ render servo top part as a moving segment, rotate as angle changes
*    3.√ remake wing controller model, make a moving part connected with wing block
*    4.  Flames of jet
*    Functionality:
*    1.√ extract ServoConstrainAssembleSchedule run() function, make it inside an class specific for ship aligning task
*    2.√ VS constrain serialize utilities
*    3.√ Make Force Inducer removing invalids by life time
*    4. Sync Animation Packet Simplify to one, Make Interface for all blocks with only one animated data
*    Features:
*    1.√ suicide block, or self-disassemble block
*    2.√ magnet block, implement using constrain or ShipForceInducer
*    3.√ Linker tool, configurable, multi-functional tool for Control Craft
*    4.√ Variants of bearings with different rotational behaviors
*    5.√ Directional Jet rudders, and rudder controller consuming liquid(optional), just like propeller controller
*    6.√ Piston with Sphere sphere_hinge connection
*    7.  Mass-adjustable block
*    User-Friendly:
*    1.  Block Placement Logic
*    2.  GUI of Client Wand
*    3.  read write of servo settings
*    Configuration:
*    1.  make more fields configurable
*/


// The value here should match an entry in the META-INF/mods.toml file
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
        modEventBus.addListener(this::addCreative);
        // Register the commonSetup method for modloading
        modEventBus.addListener(this::commonSetup);

        if(FMLEnvironment.dist == Dist.CLIENT){
            modEventBus.addListener(this::clientSetup);
        }
        modEventBus.addListener(this::addCreative);

        ControlCraftCreativeTabs.register(modEventBus);
        // ControlCraftManuals.register(modEventBus);

        ControlCraftBlocks.register();
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
        modEventBus.addListener(this::addCreative);
        // Register the commonSetup method for modloading
        modEventBus.addListener(this::commonSetup);

        if(FMLEnvironment.dist == Dist.CLIENT){
            modEventBus.addListener(this::clientSetup);
        }
        // Register ourselves for server and other game events we are interested in


        // Register the item to a creative tab
        modEventBus.addListener(this::addCreative);

        ControlCraftCreativeTabs.register(modEventBus);
        // ControlCraftManuals.register(modEventBus);

        ControlCraftBlocks.register();
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

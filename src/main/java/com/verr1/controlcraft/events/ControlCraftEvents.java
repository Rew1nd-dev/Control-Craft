package com.verr1.controlcraft.events;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.cctweaked.delegation.ComputerCraftAsyncDelegation;
import com.verr1.controlcraft.content.cctweaked.delegation.ComputerCraftDelegation;
import com.verr1.controlcraft.content.compact.tweak.impl.TweakedLinkedControllerServerHandlerExtension;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.SpeedControllerPlant;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.managers.ChunkManager;
import com.verr1.controlcraft.foundation.managers.JointHandler;
import com.verr1.controlcraft.foundation.managers.SpatialLinkManager;
import com.verr1.controlcraft.foundation.type.descriptive.MiscDescription;
import com.verr1.controlcraft.registry.ControlCraftAttachments;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.level.LevelEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.event.server.ServerStoppingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.LogicalSide;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.Mod;
import org.valkyrienskies.core.api.events.PhysTickEvent;
import org.valkyrienskies.mod.api.ValkyrienSkies;
import org.valkyrienskies.mod.common.ValkyrienSkiesMod;

@Mod.EventBusSubscriber
public class ControlCraftEvents {

    @SubscribeEvent
    public static void onServerStarting(ServerStartingEvent event) {
        // AttachmentRegistry.register();
        BlockEntityGetter.create(event.getServer());

        ControlCraftServer.INSTANCE = event.getServer();
        ControlCraftServer.OVERWORLD = event.getServer().overworld();

        JointHandler.onServerStaring(event.getServer());
        ValkyrienSkies.api().getPhysTickEvent().on(ControlCraftEvents::onPhysicsTickStart);
        ValkyrienSkies.api().getShipLoadEvent().on(ControlCraftAttachments::onShipLoad);
    }

    @SubscribeEvent
    public static void onServerTick(TickEvent.ServerTickEvent event) {
        if(event.phase == TickEvent.Phase.START){
            ControlCraftServer.SERVER_EXECUTOR.tick();
            SpatialLinkManager.tick();
            ChunkManager.tick();
            ControlCraftServer.CC_NETWORK.tick();
            BlockLinkPort.preMainTick();
            SpeedControllerPlant.ASYNC_SCHEDULER.tick();
        } else if (event.phase == TickEvent.Phase.END) {
            BlockLinkPort.postMainTick();
        }

    }

    @SubscribeEvent
    public static void onServerWorldTick(TickEvent.LevelTickEvent event) {
        if (event.phase != TickEvent.Phase.START && event.side != LogicalSide.CLIENT) {
            Level world = event.level;
            TweakedLinkedControllerServerHandlerExtension.tick(world);
        }
    }


    @SubscribeEvent
    public static void onLoadWorld(LevelEvent.Load event) {
        LevelAccessor world = event.getLevel();
        ControlCraftServer.DECIMAL_LINK_NETWORK_HANDLER.onLoadWorld(world);
    }

    @SubscribeEvent
    public static void onUnloadWorld(LevelEvent.Unload event) {
        LevelAccessor world = event.getLevel();
        ControlCraftServer.DECIMAL_LINK_NETWORK_HANDLER.onUnloadWorld(world);
    }

    @SubscribeEvent
    public static void onPlayerJoin(PlayerEvent.PlayerLoggedInEvent event){
        if(!ModList.get().isLoaded("patchouli")){
            event.getEntity().sendSystemMessage(
                    Component.literal("[Control Craft]")
                            .withStyle(s -> s.withColor(ChatFormatting.GOLD).withBold(true).withUnderlined(true))
                            .append(
                                    MiscDescription.SUGGEST_PATCHOULI.specific().stream().reduce(
                                            Component.empty(),
                                            (a, b) -> a.copy().append(Component.literal(" ")).append(b)
                                                    .withStyle(s -> s.withBold(false).withColor(ChatFormatting.AQUA))
                                    )
            ));
        }
    }

    @SubscribeEvent
    public static void onServerStopping(ServerStoppingEvent event) {
        // ConstraintCenter.onServerStopping(event.getServer());
        JointHandler.onServerStopping(event.getServer());
        BlockLinkPort.onClose();
    }

    public static void onPhysicsTickStart(PhysTickEvent event){
        JointHandler.dispatchEvent(event);

        if(event.getWorld().getDimension().equals(ValkyrienSkies.getDimensionId(ControlCraftServer.OVERWORLD))){
            ComputerCraftAsyncDelegation.onPhysTick();
            BlockLinkPort.prePhysicsTick();
        }
    }

    public static void onPhysicsTickEnd(){
        // ComputerCraftDelegation.freeDelegateThread();
    }

}

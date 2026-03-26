package com.verr1.controlcraft.mixin.qol;

import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.compact.gearwork.GearworkPlant;
import com.verr1.controlcraft.foundation.api.IOnShipBlockEntity;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.network.handler.NetworkHandler;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.gearwork.content.landinggear.LandingGearBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LandingGearBlockEntity.class)
public class MixinGearBlockEntity implements INetworkHandle, IPlant, IOnShipBlockEntity {

    @Unique
    NetworkHandler controlCraft$handler;

    @Unique
    GearworkPlant controlCraft$plant;

    @Unique
    int controlCraft$tickCounter = 0;

    @Inject(method = "<init>", at = @At("TAIL"), remap = false)
    void registerNetworkToken(
        BlockEntityType<?> type,
        BlockPos pos,
        BlockState state,
        CallbackInfo ci
    ){
        LandingGearBlockEntity gear = LandingGearBlockEntity.class.cast(this);
        controlCraft$handler = new NetworkHandler(gear);
        controlCraft$plant = new GearworkPlant(gear);
        handler().buildRegistry(SharedKeys.COMPONENT_NAME)
            .withBasic(SerializePort.of(this::controlCraft$name, this::setControlCraft$name, SerializeUtils.STRING))
            .withClient(ClientBuffer.STRING.get())
            .register();
    }

    @Inject(method = "read", at = @At("TAIL"), remap = false)
    void saveMore(CompoundTag compound, boolean clientPacket, CallbackInfo ci){
        controlCraft$handler.onRead(compound, clientPacket);
    }

    @Inject(method = "write", at = @At("TAIL"), remap = false)
    void writeMore(CompoundTag compound, boolean clientPacket, CallbackInfo ci){
        controlCraft$handler.onWrite(compound, clientPacket);
    }

    @Inject(method = "tick", at = @At("TAIL"), remap = false)
    void tickMore(CallbackInfo ci){
        if(level().isClientSide)return;
        controlCraft$tickCounter++;
        if(controlCraft$tickCounter > 10){
            tickBus();
            controlCraft$tickCounter = 0;
        }
    }

    @Unique
    public String controlCraft$name() {
        return plant().name();
    }

    @Unique
    public void setControlCraft$name(String controlCraft$name) {
        plant().withName(controlCraft$name);
    }

    @Override
    public NetworkHandler handler() {
        return controlCraft$handler;
    }

    @Override
    public @NotNull NamedComponent plant() {
        return controlCraft$plant;
    }

    @Override
    public Level level() {
        LandingGearBlockEntity self = LandingGearBlockEntity.class.cast(this);
        return self.getLevel();
    }

    @Override
    public BlockPos blockPos() {
        LandingGearBlockEntity self = LandingGearBlockEntity.class.cast(this);
        return self.getBlockPos();
    }
}

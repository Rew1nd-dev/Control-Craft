package com.verr1.controlcraft.mixin.tweak;

import com.getitemfromblock.create_tweaked_controllers.input.MouseCursorHandler;
import com.verr1.controlcraft.content.compact.tweak.TweakMouseLockState;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.valkyrienskies.core.api.ships.LoadedShip;
import org.valkyrienskies.mod.api.ValkyrienSkies;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.entity.ShipMountedToData;
import org.valkyrienskies.mod.common.entity.ShipMountedToDataProvider;

import java.util.Optional;

@Mixin(MouseCursorHandler.class)
public class MixinMouseCursorHandler {

    private static ShipMountedToData getShipMountedToData(Entity entity, float partialTicks){
        var vehicle = entity.getVehicle();
        if(vehicle == null)return null;
        if (vehicle instanceof ShipMountedToDataProvider ve) {
            return ve.provideShipMountedToData(entity, partialTicks);
        }
        var shipObjectEntityMountedTo = VSGameUtilsKt.getShipObjectManagingPos(entity.level(), ValkyrienSkies.toJOML(vehicle.position()));//entity.level().getShipObjectManagingPos(vehicle.position().toJOML()) ?: return null;
        if(shipObjectEntityMountedTo == null)return null;
        var mountedPosInShip = ValkyrienSkies.toJOML(vehicle.getPosition(partialTicks).add(0.0, vehicle.getPassengersRidingOffset() + entity.getMyRidingOffset(), 0.0));

        return new ShipMountedToData(shipObjectEntityMountedTo, mountedPosInShip);
    }

    private static LoadedShip getShipMountedTo(Entity entity){
        return Optional.ofNullable(getShipMountedToData(entity, 1)).map(ShipMountedToData::getShipMountedTo).orElse(null);
    }

    @Inject(method = "CancelPlayerTurn", at = @At("HEAD"), cancellable = true, remap = false)
    private static void controlCraft$stabilizeMountedView(CallbackInfo ci) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null || !TweakMouseLockState.isActive()) return;
        if (getShipMountedTo(player) == null) return;

        float lockedPitch = TweakMouseLockState.lockedPitch();
        float lockedYaw = TweakMouseLockState.lockedYaw();

        player.setXRot(lockedPitch);
        player.setYRot(lockedYaw);
        player.xRotO = lockedPitch;
        player.yRotO = lockedYaw;
        player.yHeadRot = lockedYaw;
        player.yHeadRotO = lockedYaw;
        player.yBodyRot = lockedYaw;
        player.yBodyRotO = lockedYaw;
        player.xBob = lockedPitch;
        player.yBob = lockedYaw;
        player.xBobO = lockedPitch;
        player.yBobO = lockedYaw;
        ci.cancel();
    }

    @Inject(method = "ActivateMouseLock", at = @At("TAIL"), remap = false)
    private static void controlCraft$captureLockedView(CallbackInfo ci) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null) {
            TweakMouseLockState.deactivate();
            return;
        }

        TweakMouseLockState.activate(player.getXRot(), player.getYRot());
    }

    @Inject(method = "DeactivateMouseLock", at = @At("TAIL"), remap = false)
    private static void controlCraft$clearLockedView(CallbackInfo ci) {
        TweakMouseLockState.deactivate();
    }
}
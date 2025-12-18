package com.verr1.controlcraft.mixin.camera;


import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.LevelRenderer;
import net.minecraft.client.renderer.culling.Frustum;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.Vec3;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.Optional;

@Mixin(LevelRenderer.class)
public class MixinLevelRenderer {


    @Unique
    private Vec3 controlCraft$cachedPos = null;

    @ModifyVariable(method = "setupRender",at = @At("STORE"), ordinal = 0)
    private double modifyX(double x) {
        Player player = Minecraft.getInstance().player;
        if(!ClientCameraManager.isLinked() || player == null)return x;
        if(controlCraft$cachedPos == null){
            controlCraft$cachedPos = Optional
                    .ofNullable(ClientCameraManager.getLinkedCamera())
                    .map(CameraBlockEntity::getCameraPosition)
                    .map(ValkyrienSkies::toMinecraft)
                    .orElse(player.position());
        }


        return controlCraft$cachedPos.x;
    }

    @ModifyVariable(method = "setupRender",at = @At("STORE"), ordinal = 1)
    private double modifyY(double y) {
        Player player = Minecraft.getInstance().player;
        if(!ClientCameraManager.isLinked() || player == null)return y;
        if(controlCraft$cachedPos == null){
            controlCraft$cachedPos = Optional
                    .ofNullable(ClientCameraManager.getLinkedCamera())
                    .map(CameraBlockEntity::getCameraPosition)
                    .map(ValkyrienSkies::toMinecraft)
                    .orElse(player.position());
        }


        return controlCraft$cachedPos.y;
    }

    @ModifyVariable(method = "setupRender",at = @At("STORE"), ordinal = 2)
    private double modifyZ(double z) {
        Player player = Minecraft.getInstance().player;
        if(!ClientCameraManager.isLinked() || player == null)return z;
        if(controlCraft$cachedPos == null){
            controlCraft$cachedPos = Optional
                    .ofNullable(ClientCameraManager.getLinkedCamera())
                    .map(CameraBlockEntity::getCameraPosition)
                    .map(ValkyrienSkies::toMinecraft)
                    .orElse(player.position());
        }


        return controlCraft$cachedPos.z;
    }




    @Inject(method = "setupRender", at = @At("RETURN"))
    private void clearCache(Camera $, Frustum $$, boolean $$$, boolean $$$$, CallbackInfo ci){
        controlCraft$cachedPos = null;
    }

}

package com.verr1.controlcraft.mixin.camera;

import com.llamalad7.mixinextras.injector.wrapoperation.Operation;
import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import com.verr1.controlcraft.content.blocks.controlchair.ControlChairBlockEntity;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.input.MountedCameraOverride;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import com.verr1.controlcraft.foundation.managers.ClientControlChairManager;
import com.verr1.controlcraft.mixinducks.ICameraDuck;
import net.minecraft.client.Camera;
import net.minecraft.client.CameraType;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.client.renderer.LevelRenderer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;
import org.joml.*;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.Unique;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;
import org.valkyrienskies.core.impl.game.ships.ShipTransformImpl;
import org.valkyrienskies.mod.common.entity.ShipMountedToData;

import java.lang.Math;
import java.util.Optional;

@Mixin(value = GameRenderer.class)
abstract class MixinGameRenderer {
    @Shadow
    @Final
    private Minecraft minecraft;
    // region Mount the camera to the ship
    @Shadow
    @Final
    private Camera mainCamera;

    @Shadow
    protected abstract double getFov(Camera camera, float f, boolean bl);

    @Shadow
    public abstract Matrix4f getProjectionMatrix(double d);

    @WrapOperation(
            method = "renderLevel",
            at = @At(
                    value = "INVOKE",
                    target = "Lnet/minecraft/client/renderer/LevelRenderer;prepareCullFrustum(Lcom/mojang/blaze3d/vertex/PoseStack;Lnet/minecraft/world/phys/Vec3;Lorg/joml/Matrix4f;)V"
            )
    )
    private void setupCameraWithMountedShip(final LevelRenderer instance, final PoseStack ignore, final Vec3 vec3,
                                            final Matrix4f matrix4f, final Operation<Void> prepareCullFrustum, final float partialTicks,
                                            final long finishTimeNano, final PoseStack matrixStack) {
        if(this.minecraft.level == null) {
            prepareCullFrustum.call(instance, matrixStack, vec3, matrix4f);
            return;
        }
        // Player player = Objects.requireNonNull(this.minecraft.player);

        // player.sendSystemMessage(Component.literal("stage_1"));
        MountedCameraOverride mountedOverride = controlCraft$mountedOverride(partialTicks);
        if (mountedOverride == null) {
            prepareCullFrustum.call(instance, matrixStack, vec3, matrix4f);
            return;
        }

        if(this.minecraft.player == null) {
            prepareCullFrustum.call(instance, matrixStack, vec3, matrix4f);
            return;
        }

        ((ICameraDuck) mainCamera).controlCraft$setupWithShipMounted(
                this.minecraft.level,
                this.minecraft.getCameraEntity() == null ? this.minecraft.player : this.minecraft.getCameraEntity(),
                mountedOverride.thirdPerson(),
                false,
                partialTicks,
                mountedOverride.clientShip(),
                mountedOverride.positionInShip(),
                mountedOverride.transformRotation()
        );

        // Apply the ship render transform to [matrixStack]
        ShipTransform renderTransform = Optional.ofNullable(mountedOverride.clientShip()).map(ClientShip::getRenderTransform).orElse(
                new ShipTransformImpl(
                        new Vector3d(),
                        new Vector3d(),
                        new Quaterniond(),
                        new Vector3d(1, 1, 1)
                )
        );


        final Quaternionf invShipRenderRotation = new Quaternionf(
                renderTransform.getShipToWorldRotation().conjugate(new Quaterniond())
        );
        matrixStack.mulPose(mountedOverride.transformRotation() ? invShipRenderRotation : new Quaternionf());

        // We also need to recompute [inverseViewRotationMatrix] after updating [matrixStack]
        final Matrix3f matrix3f = new Matrix3f(matrixStack.last().normal());
        matrix3f.invert();
        RenderSystem.setInverseViewRotationMatrix(matrix3f);

        // Camera FOV changes based on the position of the camera, so recompute FOV to account for the change of camera
        // position.
        final double fov = this.getFov(mainCamera, partialTicks, true);

        // Use [camera.getPosition()] instead of [portPos] because mounting the player to the ship has changed the camera
        // position.
        prepareCullFrustum.call(instance, matrixStack, mainCamera.getPosition(),
                this.getProjectionMatrix(Math.max(fov, this.minecraft.options.fov().get())));
    }

    @Unique
    private @Nullable MountedCameraOverride controlCraft$mountedOverride(float partialTicks) {
        CameraBlockEntity linkedCamera = ClientCameraManager.getLinkedCamera();
        if (linkedCamera != null) {
            return new MountedCameraOverride(
                    linkedCamera.getClientShip(),
                    linkedCamera.getCameraPositionShip(),
                    linkedCamera.thirdPerson(),
                    linkedCamera.transformRotation()
            );
        }

        if (!ClientControlChairManager.shouldOverrideMountedCamera()) {
            return null;
        }

        ShipMountedToData mountedData = ClientControlChairManager.mountedData(partialTicks);
        if (mountedData == null) {
            return null;
        }

        CameraType cameraType = this.minecraft.options.getCameraType();
        boolean thirdPerson = !cameraType.isFirstPerson();
        ControlChairBlockEntity chair = ClientControlChairManager.getActiveChairBlockEntity();
        ClientShip clientShip = mountedData.getShipMountedTo() instanceof ClientShip ship ? ship : null;
        Vector3dc cameraPositionInShip = new Vector3d(mountedData.getMountPosInShip());
        Entity cameraEntity = this.minecraft.getCameraEntity() == null ? this.minecraft.player : this.minecraft.getCameraEntity();
        if (cameraEntity != null) {
            cameraPositionInShip = new Vector3d(cameraPositionInShip).add(0.0, cameraEntity.getEyeHeight(), 0.0);
        }
        return new MountedCameraOverride(
                clientShip,
                cameraPositionInShip,
                thirdPerson,
                chair != null && chair.transformRotation(thirdPerson)
        );
    }

}

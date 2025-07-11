package com.verr1.controlcraft.mixin.camera;

import com.verr1.controlcraft.mixinducks.ICameraDuck;
import net.minecraft.client.Camera;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.*;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;
import org.valkyrienskies.core.impl.game.ships.ShipTransformImpl;

import java.lang.Math;
import java.util.Optional;

@Mixin(Camera.class)
public abstract class MixinCamera implements ICameraDuck {
    @Shadow
    private boolean initialized;
    @Shadow
    private BlockGetter level;
    @Shadow
    private Entity entity;
    @Shadow
    @Final
    private Vector3f forwards;
    @Shadow
    @Final
    private Vector3f up;
    @Shadow
    @Final
    private Vector3f left;
    @Shadow
    private float xRot;
    @Shadow
    private float yRot;
    @Shadow
    @Final
    private Quaternionf rotation;
    @Shadow
    private boolean detached;
    @Shadow
    private float eyeHeight;
    @Shadow
    private float eyeHeightOld;
    @Shadow
    private Vec3 position;

    @Shadow
    protected abstract double getMaxZoom(double startingDistance);

    @Shadow
    protected abstract void move(double distanceOffset, double verticalOffset, double horizontalOffset);

    @Shadow
    protected abstract void setPosition(double x, double y, double z);
    // endregion


    //Simply Coping VS camera setup functions without third person mode
    @Unique
    @Override
    public void controlCraft$setupWithShipMounted(
            final @NotNull BlockGetter level,
            final @NotNull Entity renderViewEntity,
            final boolean thirdPerson,
            final boolean thirdPersonReverse,
            final float partialTicks,
            final @Nullable ClientShip shipMountedTo,
            final @NotNull Vector3dc inShipPlayerPosition
    ) {


        ShipTransform renderTransform = Optional
                .ofNullable(shipMountedTo)
                .map(ClientShip::getRenderTransform)
                .orElse(new ShipTransformImpl(new Vector3d(), new Vector3d(), new Quaterniond(), new Vector3d(1, 1, 1)));

        final Vector3dc playerBasePos =
                renderTransform.getShipToWorldMatrix().transformPosition(inShipPlayerPosition, new Vector3d());


        this.initialized = true;
        this.level = level;
        this.entity = renderViewEntity;
        this.detached = thirdPerson;
        this.controlCraft$setRotationWithShipTransform(renderViewEntity.getViewYRot(partialTicks),
                renderViewEntity.getViewXRot(partialTicks), renderTransform);
        this.setPosition(playerBasePos.x(), playerBasePos.y(), playerBasePos.z());

    }

    @Override
    public void controlCraft$setRotationWithShipTransform(final float yaw, final float pitch, final ShipTransform renderTransform) {
        final Quaterniondc originalRotation =
                new Quaterniond().rotateY(Math.toRadians(-yaw)).rotateX(Math.toRadians(pitch)).normalize();
        final Quaterniondc newRotation =
                renderTransform.getShipCoordinatesToWorldCoordinatesRotation().mul(originalRotation, new Quaterniond());
        this.xRot = pitch;
        this.yRot = yaw;
        this.rotation.set(newRotation);
        this.forwards.set(0.0F, 0.0F, 1.0F);
        this.rotation.transform(this.forwards);
        this.up.set(0.0F, 1.0F, 0.0F);
        this.rotation.transform(this.up);
        this.left.set(1.0F, 0.0F, 0.0F);
        this.rotation.transform(this.left);
    }

    @Unique
    @Override
    public void controlCraft$setDetached(boolean detached) {
        this.detached = detached;
    }
}

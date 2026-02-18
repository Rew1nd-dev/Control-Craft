package com.verr1.controlcraft.mixin.camera;

import com.verr1.controlcraft.mixinducks.ICameraDuck;
import net.minecraft.client.Camera;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.BlockGetter;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.*;
import org.joml.primitives.AABBi;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.Unique;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;
import org.valkyrienskies.core.impl.game.ships.ShipTransformImpl;
import org.valkyrienskies.mod.common.world.RaycastUtilsKt;

import java.lang.Math;
import java.math.BigInteger;
import java.util.Objects;
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
            @NotNull Vector3dc inShipPlayerPosition,
            boolean transformRotation
    ) {
        //        Player player = Objects.requireNonNull(Minecraft.getInstance().player);
        // player.sendSystemMessage(Component.literal(thirdPerson + ", " + transformRotation + ", " + thirdPersonReverse + ", " + shipMountedTo + ", " + inShipPlayerPosition));
        ShipTransform renderTransform = Optional
                .ofNullable(shipMountedTo)
                .map(ClientShip::getRenderTransform)
                .orElse(new ShipTransformImpl(new Vector3d(), new Vector3d(), new Quaterniond(), new Vector3d(1, 1, 1)));

        Vector3dc playerBasePos =
                renderTransform.getShipToWorld().transformPosition(inShipPlayerPosition, new Vector3d());
        this.setPosition(playerBasePos.x(), playerBasePos.y(), playerBasePos.z());

        this.initialized = true;
        this.level = level;
        this.entity = renderViewEntity;
        this.detached = thirdPerson;
        if(transformRotation){
            this.controlCraft$setRotationWithShipTransform(
                    renderViewEntity.getViewYRot(partialTicks),
                    renderViewEntity.getViewXRot(partialTicks),
                    renderTransform
            );
        }else{
            this.controlCraft$setRotationWithoutShipTransform(
                    renderViewEntity.getViewYRot(partialTicks),
                    renderViewEntity.getViewXRot(partialTicks)
            );

            if(thirdPerson && shipMountedTo != null){

                final AABBi boundingBox = Optional.ofNullable((AABBi)shipMountedTo.getShipVoxelAABB()).orElse(new AABBi());

                double dist = ((boundingBox.lengthX() + boundingBox.lengthY() + boundingBox.lengthZ()) / 3.0) * 1.5;

                dist = dist > 4 ? dist : 4;

                // inShipPlayerPosition = shipMountedTo.getRenderTransform().getPositionInShip();
                playerBasePos = renderTransform.getShipToWorld().transformPosition(inShipPlayerPosition, new Vector3d());
                this.setPosition(playerBasePos.x(), playerBasePos.y(), playerBasePos.z());

                if (this.level instanceof Level) {
                    double zom = this.controlCraft$getMaxZoomIgnoringMountedShip((Level) this.level, 4.0 * (dist / 4.0), shipMountedTo);
                    this.move(-zom, zom / 3, 0.0);
                } else {
                    this.move(-this.getMaxZoom(4.0 * (dist / 4.0)), 0.0, 0.0);
                }
            }
        }





    }

    @Unique
    private double controlCraft$getMaxZoomIgnoringMountedShip(final Level level, double maxZoom,
                                                 final @NotNull ClientShip toIgnore) {
        for (int i = 0; i < 8; ++i) {
            float f = (float) ((i & 1) * 2 - 1);
            float g = (float) ((i >> 1 & 1) * 2 - 1);
            float h = (float) ((i >> 2 & 1) * 2 - 1);
            f *= 0.1F;
            g *= 0.1F;
            h *= 0.1F;
            final Vec3 vec3 = this.position.add(f, g, h);
            final Vec3 vec32 =
                    new Vec3(this.position.x - (double) this.forwards.x() * maxZoom + (double) f + (double) h,
                            this.position.y - (double) this.forwards.y() * maxZoom + (double) g,
                            this.position.z - (double) this.forwards.z() * maxZoom + (double) h);
            final HitResult hitResult = RaycastUtilsKt.vanillaClip(
                    level,
                    new ClipContext(vec3, vec32, ClipContext.Block.VISUAL, ClipContext.Fluid.NONE, this.entity)
            );
            //true,
            //toIgnore.getId()
            if (hitResult.getType() != HitResult.Type.MISS) {
                final double e = hitResult.getLocation().distanceTo(this.position);
                if (e < maxZoom) {
                    maxZoom = e;
                }
            }
        }

        return maxZoom;
    }

    @Unique
    public void controlCraft$setRotationWithShipTransform(final float yaw, final float pitch, final ShipTransform renderTransform) {
        final Quaterniondc originalRotation =
                new Quaterniond().rotateY(Math.toRadians(-yaw)).rotateX(Math.toRadians(pitch)).normalize();
        final Quaterniondc newRotation =
                renderTransform.getShipToWorldRotation().mul(originalRotation, new Quaterniond());
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
    public void controlCraft$setRotationWithoutShipTransform(final float yaw, final float pitch) {
        final Quaterniondc originalRotation =
                new Quaterniond().rotateY(Math.toRadians(-yaw)).rotateX(Math.toRadians(pitch)).normalize();
        this.xRot = pitch;
        this.yRot = yaw;
        this.rotation.set(originalRotation);
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

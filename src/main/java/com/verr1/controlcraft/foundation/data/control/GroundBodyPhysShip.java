package com.verr1.controlcraft.foundation.data.control;

import com.verr1.controlcraft.foundation.data.GroundBodyShip;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Matrix3d;
import org.joml.Matrix3dc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBd;
import org.joml.primitives.AABBdc;
import org.joml.primitives.AABBi;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.bodies.properties.BodyKinematics;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.properties.ChunkClaim;
import org.valkyrienskies.core.api.ships.properties.IShipActiveChunksSet;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;

public class GroundBodyPhysShip implements PhysShip {
    public static final GroundBodyPhysShip INSTANCE = new GroundBodyPhysShip();
    @Override
    public double getBuoyantFactor() {
        return 0;
    }

    @Override
    public boolean isStatic() {
        return true;
    }

    @Override
    public void setStatic(boolean b) {

    }

    @Override
    public void setBuoyantFactor(double v) {

    }

    @Override
    public boolean getDoFluidDrag() {
        return false;
    }

    @Override
    public void setDoFluidDrag(boolean b) {

    }

    @Override
    public double getMass() {
        return 10;
    }

    @NotNull
    @Override
    public Vector3dc getCenterOfMass() {
        return new Vector3d();
    }

    @NotNull
    @Override
    public Matrix3dc getMomentOfInertia() {
        return new Matrix3d();
    }

    @Override
    public void applyRotDependentForce(@NotNull Vector3dc vector3dc) {

    }

    @Override
    public void applyInvariantForce(@NotNull Vector3dc vector3dc) {

    }

    @Override
    public void applyRotDependentForceToPos(@NotNull Vector3dc vector3dc, @NotNull Vector3dc vector3dc1) {

    }

    @Override
    public void applyInvariantForceToPos(@NotNull Vector3dc vector3dc, @NotNull Vector3dc vector3dc1) {

    }

    @Override
    public void applyRotDependentTorque(@NotNull Vector3dc vector3dc) {

    }

    @Override
    public void applyInvariantTorque(@NotNull Vector3dc vector3dc) {

    }

    @Override
    public long getId() {
        return -1;
    }

    @Nullable
    @Override
    public String getSlug() {
        return "controlCraft$groundBody";
    }

    @NotNull
    @Override
    public BodyKinematics getKinematics() {
        return GroundBodyShip.EMPTY_KINEMATICS;
    }

    @NotNull
    @Override
    public ShipTransform getPrevTickTransform() {
        return GroundBodyShip.EMPTY_TRANSFORM;
    }

    @NotNull
    @Override
    public ChunkClaim getChunkClaim() {
        throw new UnsupportedOperationException("GroundBodyPhysShip does not support chunk claims.");
    }

    @NotNull
    @Override
    public String getChunkClaimDimension() {
        throw new UnsupportedOperationException("GroundBodyPhysShip does not support chunk claims.");
    }

    @NotNull
    @Override
    public AABBdc getWorldAABB() {
        return new AABBd();
    }

    @Nullable
    @Override
    public AABBic getShipAABB() {
        return new AABBi();
    }

    @NotNull
    @Override
    public IShipActiveChunksSet getActiveChunksSet() {
        throw new UnsupportedOperationException("GroundBodyPhysShip does not support active chunks set.");
    }
}

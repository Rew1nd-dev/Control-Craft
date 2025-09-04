package com.verr1.controlcraft.unstable.blocks.schematic;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBd;
import org.joml.primitives.AABBdc;
import org.joml.primitives.AABBi;
import org.joml.primitives.AABBic;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.ServerShipTransformProvider;
import org.valkyrienskies.core.api.ships.properties.ChunkClaim;
import org.valkyrienskies.core.api.ships.properties.IShipActiveChunksSet;
import org.valkyrienskies.core.api.ships.properties.ShipInertiaData;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;

public class DummyServerShip implements ServerShip {
    @Override
    public boolean getEnableKinematicVelocity() {
        return false;
    }

    @Nullable
    @Override
    public String getSlug() {
        return "";
    }

    @Override
    public void setSlug(@Nullable String s) {

    }

    @NotNull
    @Override
    public ShipInertiaData getInertiaData() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isStatic() {
        return true;
    }

    @Override
    public void setStatic(boolean b) {

    }

    @Override
    public void setEnableKinematicVelocity(boolean b) {

    }

    @Nullable
    @Override
    public ServerShipTransformProvider getTransformProvider() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setTransformProvider(@Nullable ServerShipTransformProvider serverShipTransformProvider) {

    }

    @Override
    public <T> void saveAttachment(@NotNull Class<T> aClass, @Nullable T t) {

    }

    @Nullable
    @Override
    public <T> T getAttachment(@NotNull Class<T> aClass) {
        return null;
    }

    @Override
    public long getId() {
        return -2L;
    }

    @NotNull
    @Override
    public ShipTransform getTransform() {
        throw new UnsupportedOperationException();
    }

    @NotNull
    @Override
    public ShipTransform getPrevTickTransform() {
        throw new UnsupportedOperationException();
    }

    @NotNull
    @Override
    public ChunkClaim getChunkClaim() {
        throw new UnsupportedOperationException();
    }

    @NotNull
    @Override
    public String getChunkClaimDimension() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setChunkClaimDimension(@NotNull String s) {

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
    public Vector3dc getVelocity() {
        return new Vector3d();
    }

    @NotNull
    @Override
    public Vector3dc getOmega() {
        return new Vector3d();
    }

    @NotNull
    @Override
    public IShipActiveChunksSet getActiveChunksSet() {
        throw new UnsupportedOperationException();
    }
}

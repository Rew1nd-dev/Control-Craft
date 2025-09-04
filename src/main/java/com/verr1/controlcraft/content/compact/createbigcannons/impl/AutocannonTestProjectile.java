package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.Level;
import rbasamoyai.createbigcannons.munitions.autocannon.flak.FlakAutocannonProjectile;

public class AutocannonTestProjectile extends FlakAutocannonProjectile {

    private boolean drag = false;

    public AutocannonTestProjectile(
            EntityType<? extends FlakAutocannonProjectile> type,
            Level level
    ) {
        super(type, level);
    }

    public boolean drag() {
        return drag;
    }

    public void setDrag(boolean drag) {
        this.drag = drag;
    }

    @Override
    protected double getDragForce() {
        return drag ? super.getDragForce() : 0.0;
    }
}

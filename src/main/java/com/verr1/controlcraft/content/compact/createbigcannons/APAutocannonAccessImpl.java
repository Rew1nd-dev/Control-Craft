package com.verr1.controlcraft.content.compact.createbigcannons;

import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.phys.Vec3;
import rbasamoyai.createbigcannons.munitions.autocannon.AbstractAutocannonProjectile;
import rbasamoyai.createbigcannons.munitions.autocannon.ap_round.APAutocannonProjectile;

import java.util.Optional;

public class APAutocannonAccessImpl implements APAutocannonAccess {
    private final AbstractAutocannonProjectile ap;

    public APAutocannonAccessImpl(AbstractAutocannonProjectile ap) {
        this.ap = ap;
    }

    @Override
    public void setPos(Vec3 pos) {
        ap.setPos(pos);
    }

    @Override
    public void setTracer(boolean tracer) {
        ap.setTracer(tracer);
    }

    @Override
    public void setLifetime(int life) {
        ap.setLifetime(life);
    }

    @Override
    public void shoot(double x, double y, double z, double vel, double spread) {
        ap.shoot(x, y, z, (float) vel, (float) spread);
    }

    @Override
    public void addToLevel() {
        Optional.of(ap.level()).ifPresent(level -> level.addFreshEntity(ap));
    }

    @Override
    public Projectile getProjectile() {
        return ap;
    }


}

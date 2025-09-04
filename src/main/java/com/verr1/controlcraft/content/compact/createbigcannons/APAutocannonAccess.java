package com.verr1.controlcraft.content.compact.createbigcannons;

import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.phys.Vec3;

public interface APAutocannonAccess {
    void setPos(Vec3 pos);

    void setTracer(boolean tracer);

    void setLifetime(int life);

    void shoot(double x, double y, double z, double vel, double spread);

    void addToLevel();

    Projectile getProjectile();

}

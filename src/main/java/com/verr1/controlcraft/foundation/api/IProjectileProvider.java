package com.verr1.controlcraft.foundation.api;

import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.item.ItemStack;

public interface IProjectileProvider {

    Projectile getProjectile(ItemStack itemStack);

}

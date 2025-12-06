package com.verr1.controlcraft.content.compact.createbigcannons;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.fml.ModList;
import org.jetbrains.annotations.Nullable;

public class CreateBigCannonsCompact {

    static ICBCCompactAccess getter;

    public static @Nullable IPeripheral cannonMountPeripheral(ServerLevel level, BlockPos pos) {
        if (getter == null) return null;
        return getter.getComputercraft(level, pos);
    }

    public static @Nullable NamedComponent cannonMountPlant(ServerLevel level, BlockPos pos) {
        if (getter == null) return null;
        return getter.getCimulink(level, pos);
    }

    public static @Nullable BlockState cannonMountBlock(int type){
        if(getter == null)return null;
        return getter.cannonMountBlock(type);
    }

    public static @Nullable APAutocannonAccess createAutocannonAp(Level level){
        if(getter == null)return null;
        return getter.createAutocannonAp(level);
    }

    public static @Nullable Projectile createHEShell(Level level){
        if(getter == null)return null;
        return getter.createHEShell(level);
    }

    public static void createExplosion(Level level, double x, double y, double z, double radius, boolean fire, Level.ExplosionInteraction interaction){
        if(getter == null)return;
        getter.createExplosion(level, x, y, z, radius, fire, interaction);
    }

    public static void init() {
        if(!ModList.get().isLoaded("createbigcannons"))return;

        try {
            Class<?> clazz = Class.forName("com.verr1.controlcraft.content.compact.createbigcannons.impl.CBCCompactAccessImpl");
            getter = (ICBCCompactAccess) clazz.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Failed to initialize CreateBigCannonsCompact", e);
        }
    }

}

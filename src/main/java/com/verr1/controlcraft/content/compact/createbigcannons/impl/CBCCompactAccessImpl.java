package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccessImpl;
import com.verr1.controlcraft.content.compact.createbigcannons.ICBCCompactAccess;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.Explosion;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;
import rbasamoyai.createbigcannons.CreateBigCannons;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;
import rbasamoyai.createbigcannons.index.CBCBlocks;
import rbasamoyai.createbigcannons.index.CBCEntityTypes;
import rbasamoyai.createbigcannons.index.CBCItems;
import rbasamoyai.createbigcannons.munitions.ShellExplosion;
import rbasamoyai.createbigcannons.munitions.autocannon.ap_round.APAutocannonProjectile;
import rbasamoyai.createbigcannons.munitions.autocannon.flak.FlakAutocannonProjectile;
import rbasamoyai.createbigcannons.munitions.big_cannon.he_shell.HEShellProjectile;
import riftyboi.cbcmodernwarfare.munitions.autocannon.he.ExplosiveAutocannonProjectile;

import java.util.concurrent.atomic.AtomicBoolean;

public class CBCCompactAccessImpl implements ICBCCompactAccess {
    @Override
    public IPeripheral getComputercraft(ServerLevel level, BlockPos pos) {
        AtomicBoolean found = new AtomicBoolean(false);
        IPeripheral ip = BlockEntityGetter.getLevelBlockEntityAt(level, pos, CannonMountBlockEntity.class)
                .map(be -> {
                    found.set(true);
                    return new CannonMountPeripheral(be);
                })
                .orElse(null);

        if(ip == null && found.get()){
            ControlCraft.LOGGER.info("cannon mount peripheral getter failed at pos: {}", pos);
        }
        return ip;
    }

    @Override
    public @Nullable NamedComponent getCimulink(ServerLevel level, BlockPos pos) {
        return BlockEntityGetter.getLevelBlockEntityAt(level, pos, ICannonDuck.class)
                .map(CannonMountPlant::new)
                .orElse(null);
    }

    @Override
    public BlockState cannonMountBlock(int type) {
        return switch (type){
            case 0 -> CBCBlocks.CANNON_MOUNT.getDefaultState();
            case 1 -> CBCBlocks.FIXED_CANNON_MOUNT.getDefaultState();
            default -> null;
        };
    }

    @Override
    public APAutocannonAccess createAutocannonAp(Level level) {
        APAutocannonProjectile ap = new APAutocannonProjectile(CBCEntityTypes.AP_AUTOCANNON.get(), level);
        AutocannonTestProjectile he = new AutocannonTestProjectile(CBCEntityTypes.FLAK_AUTOCANNON.get(), level);
        he.setDrag(false);
        he.setNoGravity(true);
        he.setFuze(CBCItems.IMPACT_FUZE.asStack());
        return new APAutocannonAccessImpl(he);
    }

    @Override
    public Projectile createHEShell(Level level) {
        HEShellProjectile ap = new HEShellProjectile(CBCEntityTypes.HE_SHELL.get(), level);
        ap.setFuze(CBCItems.IMPACT_FUZE.asStack());
        return ap;
    }

    @Override
    public Explosion createExplosion(Level level, double x, double y, double z, double radius, boolean fire, Level.ExplosionInteraction interaction) {
        ShellExplosion impact =new ShellExplosion(
                level,
                null,
                null,
                x, y, z,
                (float) radius,
                fire,
                interaction
        );
        CreateBigCannons.handleCustomExplosion(level, impact);

        return impact;
    }
}

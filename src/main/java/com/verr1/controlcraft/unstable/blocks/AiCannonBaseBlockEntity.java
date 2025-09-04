package com.verr1.controlcraft.unstable.blocks;

import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.unstable.ai.api.IAirCannon;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.projectile.Projectile;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.Objects;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;
import static com.verr1.controlcraft.unstable.ai.game.SharedAIKeys.ARROW;

public abstract class AiCannonBaseBlockEntity extends AiUtilBlockEntity implements IAirCannon {



    protected double spawnDistance = 10;

    protected int cooldown = 0;
    protected int fireRate = 0;
    protected double tol = 7.5;

    protected double spread = 0;
    protected double projectileVel = 9.0;

    public AiCannonBaseBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerDouble(this::shootTolerance, this::setShootTolerance, SharedAIKeys.TOL);
        registerDouble(this::fireRate, this::setFireRate, SharedAIKeys.FIRE_RATE);
        registerDouble(this::projectileVel, this::setProjectileVel, SharedAIKeys.PROJ_VEL);
        registerDouble(this::spread, this::setSpread, SharedAIKeys.SPREAD);
        registerDouble(this::spawnDistance, this::setSpawnDistance, SharedAIKeys.DISTANCE);

    }

    public double spawnDistance() {
        return spawnDistance;
    }

    public void setSpawnDistance(double  spawnDistance) {
        this.spawnDistance = spawnDistance;
    }

    public double spread() {
        return spread;
    }

    public void setSpread(double spread) {
        this.spread = Math.abs(spread);
    }

    private int currentRedstone = 0;

    public void tickFire(){
        if (currentRedstone > 0)fireAt(frontDirection());
    }

    public void onRedstoneUpdate(int signal){
        currentRedstone = signal;
    }

    public double projectileVel() {
        return projectileVel;
    }

    public void setProjectileVel(double projectileVel) {
        this.projectileVel = projectileVel;
    }

    @Override
    public int getCooldown() {
        return cooldown;
    }

    public void tickCooldown(){
        if(cooldown > 0)cooldown--;
    }

    public double fireRate() {
        return fireRate;
    }

    public void setFireRate(double fireRate) {
        this.fireRate = (int)fireRate;
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickCooldown();
        tickFire();
    }

    public double shootTolerance() {
        return tol;
    }

    public void setShootTolerance(double shootTolerance) {
        this.tol = shootTolerance;
    }

    public abstract Projectile getProjectile();

    @Override
    public void fireAt(Vector3dc direction) {
        if(isClientSide())return;
        if(direction.angle(frontDirection()) > Math.toRadians(tol))return;
        if(cooldown > 0)return;
        cooldown = fireRate;
        Objects.requireNonNull(level);
        Vector3dc p = getSelfPositionWorld();
        Vector3dc front = frontDirection();
        Vector3dc spawn = p.fma(spawnDistance, front, new Vector3d());
        Projectile ap = getProjectile();
        if(ap == null)return;
        ap.setPos(toMinecraft(spawn));
        ap.shoot(direction.x(), direction.y(), direction.z(), (float) projectileVel, (float) spread);
        level.addFreshEntity(ap);
    }

}

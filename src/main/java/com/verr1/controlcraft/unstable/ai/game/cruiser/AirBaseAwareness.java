package com.verr1.controlcraft.unstable.ai.game.cruiser;

import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.util.LazyRandom;
import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.levelgen.Heightmap;
import net.minecraft.world.phys.BlockHitResult;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.mod.common.world.RaycastUtilsKt;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class AirBaseAwareness {

    private final List<LazyRandom> lazyRandoms;

    // private final SchmittTrigger decision = new SchmittTrigger(-0.5, 0.5); // true: should run
    // private double threatScore = 0;
    // private double attackScore = 0;

    private double stuckScore = 0;

    private final CruiseState state = new CruiseState();




    private double max_stuck_score = 200;
    // private double max_threat_score = 1;
    private double max_attack_score = 200;

    private final Vector3d cruiseTarget = new Vector3d();

    private final Vector3d targetPosition = new Vector3d();
    private final Vector3d targetVelocity = new Vector3d();

    private final Vector3d currentPosition = new Vector3d();
    private final Vector3d currentVelocity = new Vector3d();

    private final Vector3d currentFront = new Vector3d();

    private final IAirContext context;

    private double currentHeight = 0;
    private double headingObstacleDistance = 0;

    public void tickAltitude(){
        Level world = context.world();
        if(world == null)return;
        int x = (int)currentPosition.x();
        int z = (int)currentPosition.z();
        double height = world.getHeight(Heightmap.Types.WORLD_SURFACE, x, z);
        currentHeight = currentPosition.y() - height;
        long shipId = context.shipId();
        ClipContext ctx = new ClipContext(
                toMinecraft(currentPosition),
                toMinecraft(currentPosition.fma(context.cruiseVelocity() * 3, currentFront, new Vector3d())),
                ClipContext.Block.COLLIDER,
                ClipContext.Fluid.ANY,
                null
        );

        BlockHitResult result = RaycastUtilsKt.clipIncludeShips(world, ctx, true, shipId);
        headingObstacleDistance = result.getLocation().distanceTo(toMinecraft(currentPosition));

    }


    public double currentHeight(){
        return currentHeight;
    }

    public double currentObstacleDistance(){
        return headingObstacleDistance;
    }

    public AirBaseAwareness(IAirContext context) {
        this.context = context;
        AtomicInteger delay = new AtomicInteger(0);
        lazyRandoms = ArrayUtils.ListOf(10, () -> new LazyRandom(20 * delay.getAndIncrement()));
    }

    private void tickRandoms(){
        lazyRandoms.forEach(LazyRandom::next);
    }

    public double peekRandom(int index){
        if (index < 0 || index >= lazyRandoms.size()) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + lazyRandoms.size());
        }
        return lazyRandoms.get(index).peek();
    }

    public double closingRate(){
        Vector3dc rel = targetPosition.sub(currentPosition, new Vector3d());
        Vector3dc vr = AIControlUtils.projection(targetVelocity.sub(currentVelocity, new Vector3d()), rel);
        double closing = vr.dot(rel);

        return rel.length() / (1e-8 + vr.length()) * -Math.signum(closing);
    }


//    private void tickThreat(){
//        Vector3dc relative = targetPosition.sub(currentPosition, new Vector3d());
//        double angle_t = relative.angle(targetVelocity);
//        double angle_s = relative.angle(currentFront);
//        double delta =
//                angle_t > Math.toRadians(120)
//                && angle_s > Math.toRadians(60)
//                && targetVelocity.length() > 1
//                ?
//                1 : -1;
//
//        threatScore = MathUtils.clamp(threatScore + delta * 0.05, -max_threat_score, max_threat_score);
//        decision.update(threatScore);
//    }


    private void tickStuck(){
        if(currentVelocity().length() / context.cruiseVelocity() < 0.2){
            stuckScore = MathUtils.clamp(stuckScore + 1, 0, max_stuck_score);
        }else{
            stuckScore = MathUtils.clamp(stuckScore - 3, 0, max_stuck_score);
        }
    }

    public boolean shouldDiscard(){
        return stuckScore > 0.9 * max_stuck_score;
    }

    public void resetStuckScore() {
        this.stuckScore = 0;
    }

    public double frontAngle(){
        Vector3dc relative = targetRelative();
        return relative.angle(currentFront);
    }

//    public double threatAngle(){
//        Vector3dc relative = targetPosition.sub(currentPosition, new Vector3d());
//        return relative.angle(targetVelocity);
//    }
//
//    private void tickAttack(){
//        double omega = context.cruiseVelocity() / context.cruiseRadius();
//        max_attack_score = 3 * Math.PI / omega * 20;
//
//        double angle_s = frontAngle();
//        if(angle_s < Math.toRadians(7.5)){
//            resetAttackScore();
//        }else {
//            attackScore = MathUtils.clamp(attackScore - 1, 0, max_attack_score);
//        }
//    }
//
//    public double attackScore(){
//        return attackScore;
//    }

//    public void resetAttackScore(){
//        attackScore = max_attack_score;
//    }

    public void tick(){
        lazyTick();
        tickStuck();
        // tickThreat();
        // tickAttack();
        tickRandoms();
    }

    private int lazyTickCounter = 0;
    private int lazyTickRate = 0;

    public void lazyTick(){
        if(lazyTickCounter-- > 0)return;
        lazyTickCounter = lazyTickRate;
        tickAltitude();
    }

    public void overrideTarget(
            @NotNull Vector3dc targetPosition,@NotNull  Vector3dc targetVelocity,
            @NotNull Vector3dc cruiseTarget,
            @NotNull Vector3dc currentFront
    ) {
        this.targetPosition.set(targetPosition);
        this.targetVelocity.set(targetVelocity);
        this.currentFront.set(currentFront);
        this.cruiseTarget.set(cruiseTarget);
    }

    public void overrideDual(
            @NotNull Vector3dc currentPosition,@NotNull  Vector3dc currentVelocity
    ) {
        this.currentPosition.set(currentPosition);
        this.currentVelocity.set(currentVelocity);
    }


//    public SchmittTrigger getDecision(){
//        return decision;
//    }

    public CruiseState state() {
        return state;
    }

    public Vector3d targetPosition() {
        return new Vector3d(targetPosition);
    }

    public Vector3d targetVelocity() {
        return new Vector3d(targetVelocity);
    }

    public Vector3d currentPosition() {
        return new Vector3d(currentPosition);
    }

    public Vector3dc currentVelocity() {
        return new Vector3d(currentVelocity);
    }

    public Vector3d targetRelative(){
        return targetPosition.sub(currentPosition, new Vector3d());
    }

    public boolean isInLossCone(double coneRadius){
        double dh = targetRelative().length();
        double beta = dh > coneRadius ? Math.asin(coneRadius / (dh + 1e-8) ) : Math.PI / 2;  // 0 - PI / 2
        double gamma = frontAngle();
        return gamma > Math.PI - beta;
    }

    public double distance(){
        return targetRelative().length();
    }

    public double safeDistance(){
        return Math.max(targetVelocity().length(), context.cruiseVelocity()) * 1.5 * context.extremeRadius() / context.cruiseVelocity();
    }

//    public double heuristicWindow(){
//        double gamma = frontAngle();
//        double omega = context.cruiseVelocity() / context.cruiseRadius();
//        double d = distance();
//        double v = context.cruiseVelocity();
//        double t_predict = gamma / omega + d / v;
//        Vector3dc pt_predict = targetPosition().add(targetVelocity().mul(t_predict));
//        double d_predict = pt_predict.distance(currentPosition);
//        return pt_predict.sub(currentPosition, new Vector3d()).angle(currentVelocity) * d / d_predict;
//    }


}

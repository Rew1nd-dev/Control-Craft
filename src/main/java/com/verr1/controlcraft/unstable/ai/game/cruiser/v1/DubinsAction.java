package com.verr1.controlcraft.unstable.ai.game.cruiser.v1;

import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.Status;
import com.verr1.controlcraft.unstable.ai.core.nodes.Action;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.function.Function;

public class DubinsAction extends Action {
    public static final Address<DubinsHolderV2> PATH = new Address<>("dubins_result", DubinsHolderV2.class);
    public static final Address<RandomBehaviorTimer> RAND = new Address<>("random_behavior", RandomBehaviorTimer.class);

    @Override
    protected Status perform(Blackboard blackboard) {

        /*
        * CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        if (context == null)return Status.FAILURE;

        RandomBehaviorTimer rand = blackboard.computeIfAbsent(RAND, RandomBehaviorTimer::new);
        rand.tick();

        Vector3dc pNullable = context.controller().cutoffGoal();
        Vector3dc dirNullable = context.controller().cutoffDirection();
        Vector3dc ps = context.readSelf().position();

        Vector3dc p = pNullable == null ? ps : pNullable; // if the goal is too far, use the current position
        Vector3dc dir = dirNullable == null ? new Vector3d(0, 0, 1) : dirNullable;


        if(pNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No cutoff position provided, using default.");
        }

        if(dirNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No cutoff direction provided, using default.");
        }



        Vector3dc targetPNullable = context.debug_getTarget();
        Vector3dc targetVNullable = context.debug_getTargetVelocity();

        if(targetPNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No target provided, using random.");
        }

        if(targetVNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No target direction provided, using random.");
        }

        Vector3dc randDir = rand.nextVector();
        Vector3dc randPos = randNext(20, 40).add(ps, new Vector3d());

        Vector3dc nextPosition = targetPNullable == null ? randPos : dropY(targetPNullable, y -> Math.max(y, -20));
        Vector3dc nextDirection = targetVNullable == null ? randDir :
                MathUtils.safeNormalize(
                        dropY(targetVNullable, Math::abs), //.rotateY(3 * Math.PI / 4 * random11(), new Vector3d())
                        randDir
                );

        LerpPathV2<Vector3dc> path = Navigator.dubins(
                p, dir,
                nextPosition, nextDirection,
                20
        );
        if(path == null)return Status.FAILURE;

        ControlCraft.LOGGER.info("Dubins path newed");
        blackboard.set(PATH, new DubinsHolderV2(path));
        context.controller().setNextPath(path);
        * */


        return Status.SUCCESS;
    }
    /*
    @Override
    protected Status perform(Blackboard blackboard) {

        CruiserBlockEntity context = blackboard.get(CruiserBlockEntity.CONTEXT);
        if (context == null)return Status.FAILURE;


        Vector3dc pNullable = context.controller().cutoffGoal();
        Vector3dc dirNullable = context.controller().cutoffDirection();
        Vector3dc ps = context.readSelf().position();

        Vector3dc p = pNullable == null ? ps : pNullable; // if the goal is too far, use the current position
        Vector3dc dir = dirNullable == null ? new Vector3d(0, 0, 1) : dirNullable;


        if(pNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No cutoff position provided, using default.");
        }

        if(dirNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No cutoff direction provided, using default.");
        }



        Vector3dc targetPNullable = context.debug_getTarget();

        if(targetPNullable == null){
            ControlCraft.LOGGER.info("DubinsAction: No target provided, using random.");
        }



        Vector3dc randDir = randDir3D();
        Vector3dc randPos = randNext(20, 40).add(ps, new Vector3d());

        Vector3dc nextPosition = targetPNullable == null ? randPos : dropY(targetPNullable, y -> Math.max(y, -20));

        Vector3dc delta = nextPosition.sub(ps, new Vector3d());

        Vector3dc nextDirection = MathUtils.safeNormalize(
                dropY(delta, Math::abs), //.rotateY(3 * Math.PI / 4 * random11(), new Vector3d()),
                randDir
        );


        LerpPathV2<Vector3dc> path = Navigator.dubins(
                p, dir,
                nextPosition, nextDirection,
                40
        );
        if(path == null)return Status.FAILURE;

        ControlCraft.LOGGER.info("Dubins path newed");
        blackboard.set(PATH, new DubinsHolderV2(path));
        context.controller().setNextPath(path);


        return Status.SUCCESS;
    }
    * */


    public Vector3dc dropY(Vector3dc v, Function<Double, Double> yFunc){
        return new Vector3d(v.x(), yFunc.apply(v.y()), v.z());
    }

    public static Vector3dc randNext(double min, double max){
        return randDir2D().mul(lerp(Math.random(), min, max), new Vector3d());
    }

    public static double lerp(double factor, double min, double max){
        return min + factor * (max - min);
    }

    public static double random11(){
        return 2 * Math.random() - 1;
    }

    public static Vector3dc randDir3D(){
        double theta = random11() * Math.PI;
        double phi = random11() * Math.PI / 2;
        return new Vector3d(
                Math.cos(theta) * Math.cos(phi),
                Math.sin(phi),
                Math.cos(theta) * Math.sin(phi)
        ).normalize();
    }

    public static Vector3dc randDir2D(){
        double theta = random11() * Math.PI;
        return new Vector3d(
                Math.cos(theta),
                0,
                Math.sin(theta)
        ).normalize();
    }

    public static class RandomBehaviorTimer{
        int tick = 0;

        double thisRandom = 0;
        Vector3dc thisRandomVector = new Vector3d();

        public double next(){
            return thisRandom;
        }

        public Vector3dc nextVector(){
            return thisRandomVector;
        }

        public void tick(){
            if(tick-- < 0){
                tick = 10;
                thisRandomVector = randDir3D();
                thisRandom = random11();
            }
        }

    }

}

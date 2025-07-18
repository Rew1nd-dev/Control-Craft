package com.verr1.controlcraft.content.valkyrienskies.controls;

import com.verr1.controlcraft.config.BlockPropertyConfig;
import com.verr1.controlcraft.content.valkyrienskies.attachments.Observer;
import com.verr1.controlcraft.foundation.data.ShipPhysics;
import com.verr1.controlcraft.foundation.data.control.ImmutablePhysPose;
import com.verr1.controlcraft.foundation.data.logical.*;
import com.verr1.controlcraft.foundation.vsapi.PhysShipWrapper;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import net.minecraft.core.Direction;
import org.jetbrains.annotations.NotNull;
import org.joml.*;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.physics_api.PoseVel;

import java.lang.Math;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;

/*
*   This is what makes Control Craft to be Control Craft :)
*
* */

public class InducerControls {

    public static void anchorTickControls(LogicalAnchor anchor, @NotNull PhysShipWrapper physShip) {

        Vector3dc p_sc = ValkyrienSkies.set(new Vector3d(), anchor.pos().pos().getCenter());
        Vector3dc s_sc = physShip.getTransform().getPositionInShip();
        Vector3dc r_sc = p_sc.sub(s_sc, new Vector3d());
        Vector3dc r_sc_resistance = anchor.airResistAtPos() ? r_sc : new Vector3d();
        Vector3dc r_sc_gravity = anchor.extraGravityAtPos() ? r_sc : new Vector3d();

        Vector3dc velocity = physShip.getVelocity();
        Vector3dc omega = physShip.getOmega();
        Vector3dc abs_velocity = velocity.add(omega.cross(r_sc_resistance, new Vector3d()), new Vector3d());

        Vector3dc v_dir = MathUtils.safeNormalize(abs_velocity);
        double v_scale = abs_velocity.length();

        double scale = anchor.squareDrag() ? v_scale * v_scale : v_scale;

        Vector3dc fAirResistance = v_dir.mul(scale, new Vector3d()).mul(-anchor.airResist() * physShip.getMass());
        Vector3dc fExtraGravity = new Vector3d(0, -physShip.getMass(), 0).mul(anchor.extraGravity());


        double ts = 0.01667;
        int id = physShip.getTransform().getShipToWorldScaling().minComponent();
        // double scale = physShip.getTransform().getShipToWorldScaling().get(id);
        double inertia = physShip.getMomentOfInertia().m00();

        Vector3dc q_d = physShip.getAngularVelocity();
        Vector3dc accel_d = new Vector3d(q_d.x(), q_d.y(), q_d.z()).mul(-2 / ts).mul(anchor.rotDamp());
        Vector3dc tRotationalResistance = new Vector3d(accel_d).mul(inertia);

        physShip.applyInvariantForceToPos(fExtraGravity, r_sc_gravity);
        physShip.applyInvariantForceToPos(fAirResistance, r_sc_resistance);
        physShip.applyInvariantTorque(tRotationalResistance);



    }

    private static double scaleOf(Vector3dc scaleVector){
        return scaleVector.get(scaleVector.minComponent());
    }

    public static void dynamicMotorTickControls(LogicalDynamicMotor motor, @NotNull  PhysShipWrapper motorShip, @NotNull PhysShipWrapper compShip) {
        if(!motor.free())return;


        double angle = VSMathUtils.get_yc2xc(motorShip, compShip, motor.motorDir(), motor.compDir());
        double speed = VSMathUtils.get_dyc2xc(motorShip, compShip, motorShip.getOmega(), compShip.getOmega(), motor.motorDir(), motor.compDir());

        motor.speedCallBack().accept(speed);
        motor.angleCallBack().accept(angle);

        double metric = motor.angleOrSpeed() ? angle : speed;

        double accel_clamp = 1000;

        double accel_scale = VSMathUtils.clamp(motor.controller()
                .calculateWithLimitContext(
                        motor.angleOrSpeed(),
                        motor.speedLimit(),
                        speed
                ),
                accel_clamp
        );
        double control_torque = motor.torque();


        double internal_torque = compShip.getMomentOfInertia().m00() * accel_scale; // * scale_5;
        Vector3dc direction = ValkyrienSkies.set(new Vector3d(), motor.motorDir().getNormal());
        Vector3dc controlTorque_sc = direction.mul((-control_torque + internal_torque) * -1   , new Vector3d()); //
        Vector3dc controlTorque_wc = VSMathUtils.get_sc2wc(motorShip).transform(controlTorque_sc, new Vector3d());

        motor.controller().overrideError(metric);

        compShip.applyInvariantTorque(controlTorque_wc);
        if(motor.eliminateGravity())compShip.applyInvariantForce(new Vector3d(0, compShip.getMass() * 10, 0));
        if(motor.shouldCounter())   motorShip.applyInvariantTorque(controlTorque_wc.mul(-1, new Vector3d()));
    }


    public static void sliderTickControls(LogicalSlider slider, @NotNull PhysShipWrapper selfShip, @NotNull PhysShipWrapper compShip){
        if(!slider.free())return;
        Vector3dc own_local_pos = slider.selfContact();
        Vector3dc cmp_local_pos = slider.compContact();

        Matrix4dc own_s2w = selfShip.getTransform().getShipToWorld();
        Matrix4dc own_w2s = selfShip.getTransform().getWorldToShip();
        Matrix4dc cmp_s2w = compShip.getTransform().getShipToWorld();

        Vector3dc own_wc = own_s2w.transformPosition(own_local_pos, new Vector3d());
        Vector3dc cmp_wc = cmp_s2w.transformPosition(cmp_local_pos, new Vector3d());
        Vector3dc sub_sc = own_w2s
                .transformDirection(
                        cmp_wc.sub(own_wc, new Vector3d()), new Vector3d()
                );

        Direction dir = slider.slideDir();
        double sign = dir.getAxisDirection() == Direction.AxisDirection.POSITIVE ? 1 : -1;
        double distance = switch (dir.getAxis()){
            case X -> sign * sub_sc.x();
            case Y -> sign * sub_sc.y();
            case Z -> sign * sub_sc.z();
        };


        double metric = slider.positionOrSpeed() ?
                distance :
                VSMathUtils.get_dy2x_sc(selfShip, compShip, slider.slideDir());

        double extra = slider.force();
        double mass = compShip.getMass();
        double scale = slider.controller().calculateControlValueScaleLinear();
        double clampedScale = VSMathUtils.clamp(scale, 1000);

        double m_scale = scaleOf(compShip.getTransform().getShipToWorldScaling());
        double scale_5 = Math.pow(m_scale, 5);
        double scale_3 = Math.pow(m_scale, 3);

        slider.controller().overrideError(metric);
        Vector3dc dirJOML = ValkyrienSkies.set(new Vector3d(), dir.getNormal());
        Vector3dc dirJOML_wc = own_s2w.transformDirection(new Vector3d(dirJOML));
        double cos = dirJOML_wc.angleCos(new Vector3d(0, 1, 0));
        Vector3dc controlForce_sc = dirJOML.mul((clampedScale + cos * 10) * mass + extra, new Vector3d()).mul(scale_3);
        Vector3dc controlForce_wc = own_s2w.transformDirection(controlForce_sc, new Vector3d());

        Vector3dc own_r = new Vector3d(own_local_pos).sub(selfShip.getTransform().getPositionInShip());
        Vector3dc cmp_r = new Vector3d(cmp_local_pos).sub(compShip.getTransform().getPositionInShip());

        compShip.applyInvariantForceToPos(controlForce_wc.mul( 1, new Vector3d()), cmp_r);
        if(!slider.shouldCounter())return;
        selfShip.applyInvariantForceToPos(controlForce_wc.mul(-1, new Vector3d()), own_r);

    }

    public static void spatialTickControls(LogicalSpatial spatial, @NotNull PhysShipWrapper physShip){
        if(!spatial.shouldDrive())return;
        spatial.schedule().overridePhysics(physShip);
        Vector3dc controlTorque = spatial.schedule().calcControlTorque();
        Vector3dc controlForce  = spatial.schedule().calcControlForce();

        physShip.applyInvariantForce(controlForce);
        physShip.applyInvariantTorque(controlTorque);
    }

    public static void jetTickControls(LogicalJet jet, @NotNull PhysShipWrapper physShip) {
        Vector3dc dir = jet.direction();
        double thrust = MathUtils.clamp(jet.thrust(), BlockPropertyConfig._JET_MAX_THRUST);
        if(!BlockPropertyConfig._CAN_JET_THRUST_BACK)thrust = MathUtils.relu(thrust);


        Vector3dc force_sc = dir.mul(thrust, new Vector3d());
        Vector3dc force_wc = physShip.getTransform().getShipToWorld().transformDirection(force_sc, new Vector3d());

        Vector3dc ship_sc = physShip.getTransform().getPositionInShip();
        Vector3dc jet_sc = ValkyrienSkies.set(new Vector3d(), jet.pos().pos().getCenter());
        Vector3dc relativeRadius_sc = jet_sc.sub(ship_sc, new Vector3d());

        physShip.applyInvariantForceToPos(force_wc, relativeRadius_sc);
    }

    public static void propellerTickControls(LogicalPropeller propeller, @NotNull PhysShipWrapper physShip) {
        if(!propeller.canDrive())return;
        Vector3dc p_sc = ValkyrienSkies.set(new Vector3d(), propeller.pos().pos().getCenter());
        Vector3dc s_sc = physShip.getTransform().getPositionInShip();
        Vector3dc r_sc = p_sc.sub(s_sc, new Vector3d());

        double thrust_abs = MathUtils.clamp(propeller.speed() * propeller.THRUST_RATIO(), BlockPropertyConfig._PROPELLER_MAX_THRUST);
        double torque_abs = MathUtils.clamp(propeller.speed() * propeller.TORQUE_RATIO(), BlockPropertyConfig._PROPELLER_MAX_TORQUE);

        Vector3dc torque = new Vector3d(propeller.direction()).mul(torque_abs);
        Vector3dc thrust = new Vector3d(propeller.direction()).mul(thrust_abs);

        Vector3dc torque_wc = physShip.getTransform().getShipToWorld().transformDirection(torque, new Vector3d());
        Vector3dc thrust_wc = physShip.getTransform().getShipToWorld().transformDirection(thrust, new Vector3d());


        physShip.applyInvariantForceToPos(thrust_wc, r_sc);
        physShip.applyInvariantTorque(torque_wc);
    }


    public static ImmutablePhysPose kinematicMotorTickControls(LogicalKinematicMotor motor, Ship motorShip, ServerShip compShip){
        Quaterniondc q_m = motorShip.getTransform().getShipToWorldRotation();
        Quaterniondc q_m_c = motor.context().self().getRot();
        Quaterniondc q_c_c = motor.context().comp().getRot();
        double AngleFix = VSMathUtils.getDumbFixOfLockMode(motor.servoDir(), motor.compAlign());
        double target = motor.controller().getTarget();
        Quaterniondc q_e = new Quaterniond().rotateAxis(
                AngleFix - target,
                ValkyrienSkies.set(new Vector3d(), motor.compAlign().getNormal())
        );

        Quaterniondc q_t = new Quaterniond(q_m)
                .mul(q_m_c.conjugate(new Quaterniond()))
                .mul(q_c_c)
                .mul(q_e)
                .normalize();

        ShipPhysics comp_sp = Observer.getOrCreate(compShip).read(); // fixing ServerShip::getPositionInShip() not updating when new blocks placed

        Vector3dc p_m_contact_s = motor.context().self().getPos();
        Vector3dc p_m_contact_w = motorShip.getTransform().getShipToWorld().transformPosition(p_m_contact_s, new Vector3d());
        Vector3dc p_c_contact_s = motor.context().comp().getPos();
        Vector3dc r_c_contact_s = p_c_contact_s.sub(compShip
                                                    .getInertiaData()
                                                    .getCenterOfMassInShip()
                                                    .add(new Vector3d(0.5, 0.5, 0.5),
                                                            new Vector3d()),
                                                    new Vector3d());  //comp_sp.positionInShip()
        Vector3dc r_c_contact_w = q_t.transform(r_c_contact_s, new Vector3d());
        Vector3dc p_t = p_m_contact_w.sub(r_c_contact_w, new Vector3d());

        // compShip.setKinematicTarget(new Ei(p_t, q_t));
        if(motor.angleOrSpeed()){
            motor.controller().updateForcedTarget();
        }else{
            motor.controller().updateTargetAngular(1d / 60);
        }
        // ((PhysShipImpl)compShip).setKinematicTarget(new Ei(p_t, q_t));
        return ImmutablePhysPose.of(p_t, q_t);

    }

    public static void kinematicMotorTickControls(LogicalKinematicMotor motor, PhysShipWrapper motorShip, PhysShipWrapper compShip){
        Quaterniondc q_m = motorShip.getTransform().getShipToWorldRotation();
        Quaterniondc q_m_c = motor.context().self().getRot();
        Quaterniondc q_c_c = motor.context().comp().getRot();
        double AngleFix = VSMathUtils.getDumbFixOfLockMode(motor.servoDir(), motor.compAlign());
        double target = motor.controller().getTarget();
        Quaterniondc q_e = new Quaterniond().rotateAxis(
                AngleFix - target,
                ValkyrienSkies.set(new Vector3d(), motor.compAlign().getNormal())
        );

        Quaterniondc q_t = new Quaterniond(q_m)
                .mul(q_m_c.conjugate(new Quaterniond()))
                .mul(q_c_c)
                .mul(q_e)
                .normalize();

        // ShipPhysics comp_sp = Observer.getOrCreate(compShip).read(); // fixing ServerShip::getPositionInShip() not updating when new blocks placed

        Vector3dc p_m_contact_s = motor.context().self().getPos();
        Vector3dc p_m_contact_w = motorShip.getTransform().getShipToWorld().transformPosition(p_m_contact_s, new Vector3d());
        Vector3dc p_c_contact_s = motor.context().comp().getPos();
        Vector3dc r_c_contact_s = p_c_contact_s.sub(compShip.getTransform().getPositionInShip().add(new Vector3d(0.5, 0.5, 0.5), new Vector3d()), new Vector3d());  //comp_sp.positionInShip()
        Vector3dc r_c_contact_w = q_t.transform(r_c_contact_s, new Vector3d());
        Vector3dc p_t = p_m_contact_w.sub(r_c_contact_w, new Vector3d());

        // compShip.setKinematicTarget(new Ei(p_t, q_t));
        if(motor.angleOrSpeed()){
            motor.controller().updateForcedTarget();
        }else{
            motor.controller().updateTargetAngular(1d / 60);
        }
        compShip.implOptional().ifPresent(impl ->
        {
            impl.setEnableKinematicVelocity(true);
            impl.setStatic(true);
            impl.setPoseVel(new PoseVel(p_t, q_t, new Vector3d(), new Vector3d()));
        });

    }


    public static void flapTickControls(LogicalFlap flap, PhysShipWrapper ship){
        double lift = flap.lift();
        double drag = flap.drag();

        Vector3dc p_sc = ship.getTransform().getPositionInShip();
        Vector3dc r_sc = toJOML(flap.posInShip().getCenter()).sub(p_sc, new Vector3d());

        Vector3dc v_wc = ship.getVelocity();
        Vector3dc w_wc = ship.getAngularVelocity();
        Vector3dc r_wc = ship.getTransform().getShipToWorld().transformDirection(r_sc, new Vector3d());
        Vector3dc n_wc = ship.getTransform().getShipToWorld().transformDirection(flap.normal(), new Vector3d());

        Vector3dc rv_wc = v_wc.add(w_wc.cross(r_wc, new Vector3d()), new Vector3d());
        Vector3dc pj_wc = rv_wc.sub(
                n_wc.mul(n_wc.dot(rv_wc), new Vector3d()), new Vector3d()
        );

        if(pj_wc.lengthSquared() < 1.0E-12 || rv_wc.lengthSquared() < 1.0E-12)return;

        Vector3dc pj_d_wc = pj_wc.normalize(new Vector3d());

        double angle = pj_d_wc.angle(rv_wc) * -Math.signum(n_wc.dot(rv_wc));

        Vector3dc drag_d_wc = new Vector3d(rv_wc).mul(-1.0).normalize();

        double s2a = Math.sin(2 * angle);
        double lift_scale = MathUtils.clamp(lift * s2a * pj_wc.lengthSquared(), 1.0E7);
        Vector3dc lift_wc = n_wc.mul(lift_scale, new Vector3d());

        double c2a = 1 - Math.cos(2 * angle);
        double drag_scale = MathUtils.clamp(drag * c2a * rv_wc.lengthSquared(), 1.0E7);
        Vector3dc drag_wc = drag_d_wc.mul(drag_scale, new Vector3d());

        Vector3dc combine_wc = lift_wc.add(drag_wc, new Vector3d());
        Vector3dc torque_wc  = r_wc.cross(combine_wc, new Vector3d());

        ship.applyInvariantForce(combine_wc);
        ship.applyInvariantTorque(torque_wc);
    }

}

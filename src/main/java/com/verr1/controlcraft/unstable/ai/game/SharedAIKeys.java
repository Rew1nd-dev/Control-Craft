package com.verr1.controlcraft.unstable.ai.game;

import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.unstable.ai.api.*;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.game.cruiser.AirBaseAwareness;

public class SharedAIKeys {

    public static final NetworkKey VEL = NetworkKey.create("cruiser_vel");
    public static final NetworkKey GOAL = NetworkKey.create("cruiser_goal");
    public static final NetworkKey PATH = NetworkKey.create("cruiser_path");
    public static final NetworkKey RAD = NetworkKey.create("cruiser_rad");
    public static final NetworkKey E_RAD = NetworkKey.create("extreme_rad");
    public static final NetworkKey TAR = NetworkKey.create("db_tar");
    public static final NetworkKey ARROW = NetworkKey.create("db_arrow");

    public static final NetworkKey TOL = NetworkKey.create("shoot_tol");
    public static final NetworkKey FIRE_RATE = NetworkKey.create("fire_rate");
    public static final NetworkKey TWI = NetworkKey.create("cruise_twist");
    public static final NetworkKey YAW = NetworkKey.create("cruise_yaw");
    public static final NetworkKey ACTUAL_FLIGHT = NetworkKey.create("actual_flight");
    public static final NetworkKey ACTUAL_WEAPON = NetworkKey.create("actual_weapon");
    public static final Address<AirBaseAwareness> AWARENESS = new Address<>("awareness", AirBaseAwareness.class);
    public static Address<IFighterJetContext> FIGHTER_CONTEXT = new Address<>("cruiser", IFighterJetContext.class);
    public static Address<IAttackerContext> ATTACKER_CONTEXT = new Address<>("attacker", IAttackerContext.class);


    public static Address<ICircleContext> CIRCLE_CONTEXT = new Address<>("circle", ICircleContext.class);
    public static Address<IAnchorContext> ANCHOR_CONTEXT = new Address<>("anchor", IAnchorContext.class);

    public static Address<IAirContext> AIR_COMMON = new Address<>("cruiser", IAirContext.class);
    public static Address<Double> EVADE_TARGET_ANGLE = new Address<>("evade_target_angle", Double.class);

    public static final NetworkKey P_DRIVE = NetworkKey.create("p_drive");
    public static final NetworkKey I_DRIVE = NetworkKey.create("i_drive");
    public static final NetworkKey TURN_RESIST = NetworkKey.create("turn_resist");

}

package com.verr1.controlcraft.unstable.ai.game;

import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.unstable.ai.api.IAirContext;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;

public class SharedAIKeys {

    public static final NetworkKey VEL = NetworkKey.create("cruiser_vel");
    public static final NetworkKey GOAL = NetworkKey.create("cruiser_goal");
    public static final NetworkKey PATH = NetworkKey.create("cruiser_path");
    public static final NetworkKey RAD = NetworkKey.create("cruiser_rad");
    public static final NetworkKey TAR = NetworkKey.create("db_tar");
    public static final NetworkKey TOL = NetworkKey.create("shoot_tol");
    public static final NetworkKey TWI = NetworkKey.create("cruise_twist");
    public static final NetworkKey YAW = NetworkKey.create("cruise_yaw");
    public static final NetworkKey ACTUAL_FLIGHT = NetworkKey.create("actual_flight");
    public static final NetworkKey ACTUAL_WEAPON = NetworkKey.create("actual_weapon");
    public static Address<IAirContext> CONTEXT = new Address<>("cruiser", IAirContext.class);
}

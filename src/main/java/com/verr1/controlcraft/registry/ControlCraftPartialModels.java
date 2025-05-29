package com.verr1.controlcraft.registry;

import com.jozufozu.flywheel.core.PartialModel;
import com.verr1.controlcraft.ControlCraft;

public class ControlCraftPartialModels {
    public static final PartialModel
        NORMAL_PROPELLER = block("simple_propeller"),
        NORMAL_PROPELLER_CENTER = block("simple_propeller_center"),
        SPINALYZR_AXES = block("spinalyzer_axes") ,
        SERVO_TOP = block("servo_top"),
        CONSTRAINT_SERVO_TOP = block("constraint_revolute_top"),
        WING_CONTROLLER_TOP = block("wing_controller_top"),
        WING_CONTROLLER_TOP_O = block("wing_controller_top_o"),
        CAMERA_LENS = block("camera_lens"),
        CAMERA_YAW = block("camera_yaw"),
        RUDDER_PART = block("rudder"),

        SPATIAL_CORE = block("spatial_core"),

        SLIDER_TOP = block("slider_top"),
        CONSTRAINT_SLIDER_TOP = block("constraint_slider_top"),
        SLIDER_PILLAR = block("slider_pillar");




    private static PartialModel block(String path) {
        return new PartialModel(ControlCraft.asResource("block/" + path));
    }


    public static void init() {
        // init static fields
    }
}

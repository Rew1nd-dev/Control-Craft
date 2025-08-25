package com.verr1.controlcraft.unstable.valkyrienskies.context;

public record LogicalDirectionTarget(
        CruiseController controller,
        PoseController poseController,
        double p_drive
) { }

package com.verr1.controlcraft.unstable.valkyrienskies.context;

public record LogicalDirectionTarget(CruiserControllerV4 controller,
                                     PoseController poseController,
                                     boolean disableDirectControl) {
}

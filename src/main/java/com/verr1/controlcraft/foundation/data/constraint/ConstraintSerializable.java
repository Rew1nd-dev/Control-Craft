package com.verr1.controlcraft.foundation.data.constraint;

import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;
import org.valkyrienskies.core.apigame.joints.*;

public record ConstraintSerializable(VSJoint constraint) {

    public static ConstraintSerializable deserialize(CompoundTag tag) {
        String type = tag.getString("type");
        CompoundTag nbt = tag.getCompound("nbt");
        VSJoint joint =
        switch (VSJointType.valueOf(type)) {
            case FIXED -> SerializeUtils.FIXED_JOINT.deserialize(nbt);
            case REVOLUTE -> SerializeUtils.REVOLUTE_JOINT.deserialize(nbt);
            case PRISMATIC -> SerializeUtils.PRISMATIC_JOINT.deserialize(nbt);
            case DISTANCE -> SerializeUtils.DISTANCE_JOINT.deserialize(nbt);
            case SPHERICAL -> SerializeUtils.SPHERICAL_JOINT.deserialize(nbt);
            default -> throw new IllegalStateException("Unexpected value: " + type);
        };
        return new ConstraintSerializable(joint);
    }

    public CompoundTag serialize() {
        CompoundTag tag = new CompoundTag();
        tag.putString("type", constraint.getJointType().name());
        CompoundTag constraintTag =
        switch (constraint.getJointType()) {
            case FIXED -> SerializeUtils.FIXED_JOINT.serialize((VSFixedJoint) constraint);
            case REVOLUTE -> SerializeUtils.REVOLUTE_JOINT.serialize((VSRevoluteJoint) constraint);
            case DISTANCE -> SerializeUtils.DISTANCE_JOINT.serialize((VSDistanceJoint) constraint);
            case PRISMATIC -> SerializeUtils.PRISMATIC_JOINT.serialize((VSPrismaticJoint) constraint);
            case SPHERICAL -> SerializeUtils.SPHERICAL_JOINT.serialize((VSSphericalJoint) constraint);
            default -> throw new IllegalStateException("Unexpected value: " + constraint.getJointType().name());
        };
        tag.put("nbt", constraintTag);
        return tag;
    }

}

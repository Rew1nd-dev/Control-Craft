package com.verr1.controlcraft.content.blocks.joints;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.utils.VSGetterUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.joml.Quaterniond;
import org.joml.Vector3dc;
import org.valkyrienskies.core.apigame.joints.VSJoint;
import org.valkyrienskies.core.apigame.joints.VSJointMaxForceTorque;
import org.valkyrienskies.core.apigame.joints.VSJointPose;
import org.valkyrienskies.core.apigame.joints.VSSphericalJoint;

public class FreeJointBlockEntity extends AbstractJointBlockEntity{
    public FreeJointBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerConstraintKey("fix");
    }

    @Override
    public void destroyConstraints() {
        removeConstraint("fix");
    }

    @Override
    public void bruteDirectionalConnectWith(BlockPos pos, Direction align, Direction forward) {
        if(level == null || level.isClientSide)return;

        // if(otherShip == null || selfShip == null)return;
        FreeJointBlockEntity otherHinge = BlockEntityGetter.getLevelBlockEntityAt(level, pos, FreeJointBlockEntity.class).orElse(null);
        if(otherHinge == null)return;

        Vector3dc selfContact = getJointConnectorPosJOML();
        Vector3dc otherContact = otherHinge.getJointConnectorPosJOML();

        long selfID = getShipOrGroundID();
        long otherID = otherHinge.getShipOrGroundID();



        VSSphericalJoint joint = new VSSphericalJoint(
                selfID,
                new VSJointPose(selfContact, new Quaterniond()),
                otherID,
                new VSJointPose(otherContact, new Quaterniond()),
                new VSJointMaxForceTorque(1e20f, 1e20f),
                null
        );

        recreateConstraints(joint);
    }

    public void recreateConstraints(@NotNull VSJoint... joint){
        if(level == null || level.isClientSide)return;
        if(joint.length == 0){
            ControlCraft.LOGGER.error("invalid constraint data for free joint");
            return;
        }
        overrideConstraint("fix", joint[0]);

    }

    @Override
    public Direction getAlign() {
        return Direction.UP;
    }

    @Override
    public Direction getForward() {
        return Direction.NORTH;
    }
}

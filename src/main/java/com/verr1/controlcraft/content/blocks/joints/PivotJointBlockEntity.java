package com.verr1.controlcraft.content.blocks.joints;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.VSGetterUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.AxisAngle4d;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3dc;
import org.valkyrienskies.core.apigame.joints.VSJoint;
import org.valkyrienskies.core.apigame.joints.VSJointMaxForceTorque;
import org.valkyrienskies.core.apigame.joints.VSJointPose;
import org.valkyrienskies.core.apigame.joints.VSRevoluteJoint;

public class PivotJointBlockEntity extends AbstractJointBlockEntity{
    public PivotJointBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerConstraintKey("revolute");
        registerConstraintKey("attach");
    }

    @Override
    public void destroyConstraints() {
        removeConstraint("revolute");
        removeConstraint("attach");
    }

    public Direction getJointDirection(){
        return getDirection();
    }

    @Override
    public void bruteDirectionalConnectWith(BlockPos pos, Direction align, Direction forward) {
        if(level == null || level.isClientSide)return;

        PivotJointBlockEntity otherHinge = BlockEntityGetter.getLevelBlockEntityAt(level, pos, PivotJointBlockEntity.class).orElse(null);
        if(otherHinge == null)return;

        Vector3dc selfContact = getJointConnectorPosJOML();
        Vector3dc otherContact = otherHinge.getJointConnectorPosJOML();

        Quaterniondc selfRotation = new
                Quaterniond(VSMathUtils.getQuaternionOfPlacement(getJointDirection()))
                .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)), new Quaterniond())
                .normalize();

        Quaterniondc otherRotation = new
                Quaterniond(VSMathUtils.getQuaternionOfPlacement(otherHinge.getJointDirection().getOpposite()))
                .mul(new Quaterniond(new AxisAngle4d(Math.toRadians(90.0), 0.0, 0.0, 1.0)), new Quaterniond())
                .normalize();

        long selfID = getShipOrGroundID();
        long otherID = otherHinge.getShipOrGroundID();

        VSRevoluteJoint joint = new VSRevoluteJoint(
                selfID,
                new VSJointPose(selfContact, selfRotation),
                otherID,
                new VSJointPose(otherContact, otherRotation),
                new VSJointMaxForceTorque(1e20f, 1e20f),
                null, null, null, null, null
        );

        recreateConstraints(joint);
    }

    public void recreateConstraints(VSJoint... joint){
        if(level == null || level.isClientSide)return;
        if(joint.length < 1){
            ControlCraft.LOGGER.error("invalid constraint data for pivot joint");
            return;
        }
        overrideConstraint("revolute", joint[0]);
    }

    @Override
    public Direction getAlign() {
        return getDirection();
    }

    @Override
    public Direction getForward() {
        return MinecraftUtils.getVerticalDirectionSimple(getDirection());
    }
}

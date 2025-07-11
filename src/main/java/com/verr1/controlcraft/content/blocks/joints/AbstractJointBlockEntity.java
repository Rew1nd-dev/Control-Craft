package com.verr1.controlcraft.content.blocks.joints;

import com.verr1.controlcraft.content.blocks.ShipConnectorBlockEntity;
import com.verr1.controlcraft.foundation.api.operatable.IAdjustableJoint;
import com.verr1.controlcraft.foundation.api.operatable.IBruteConnectable;
import com.verr1.controlcraft.foundation.api.operatable.IConstraintHolder;
import com.verr1.controlcraft.foundation.type.JointLevel;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.valkyrienskies.mod.api.ValkyrienSkies;

public abstract class AbstractJointBlockEntity extends ShipConnectorBlockEntity implements
        IConstraintHolder, IAdjustableJoint, IBruteConnectable
{
    public AbstractJointBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }


    public JointLevel getAdjustment(){
        return getBlockState().getValue(AbstractJointBlock.LEVEL);
    }

    protected Vector3d getJointConnectorPosJOML() {
        Vector3d original = ValkyrienSkies.set(new Vector3d(), getBlockPos().getCenter())
                .fma(-0.5, getDirectionJOML())
                .fma(getAdjustment().length(), getDirectionJOML());

        return isOnShip() ? original : original.add(new Vector3d(0.5, 0.5, 0.5));

    }


    @Override
    public void setAdjustment(JointLevel level) {
        MinecraftUtils.updateBlockState(this.level, getBlockPos(), getBlockState().setValue(AbstractJointBlock.LEVEL, level));
    }

}

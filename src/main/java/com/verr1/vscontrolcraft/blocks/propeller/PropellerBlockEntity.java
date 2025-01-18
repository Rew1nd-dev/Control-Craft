package com.verr1.vscontrolcraft.blocks.propeller;

import com.simibubi.create.foundation.blockEntity.SmartBlockEntity;
import com.simibubi.create.foundation.blockEntity.behaviour.BlockEntityBehaviour;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.vscontrolcraft.base.PacketHandler;
import com.verr1.vscontrolcraft.base.ISyncable;
import com.verr1.vscontrolcraft.registry.AllPackets;
import com.verr1.vscontrolcraft.utils.Util;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.network.PacketDistributor;

import java.util.List;

/*
* TODO:
*  1. add some propeller classes that extends this class
*  2. make propeller stat changeable (GUI)
* */

public class PropellerBlockEntity extends SmartBlockEntity implements ISyncable{
    public double ThrustRatio = 1000;
    public double TorqueRatio = 1000;
    public boolean ReverseTorque = false;
    public double rotationalSpeed = 5;

    public LerpedFloat angle;
    public float targetAngle = 0;

    public PropellerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        angle = LerpedFloat.angular();
        lazyTickRate = 3;
    }

    public double getThrustRatio() {
        return ThrustRatio;
    }

    public double getTorqueRatio(){
        return TorqueRatio;
    }

    public boolean getReverseTorque(){
        return ReverseTorque;
    }

    @Override
    public void addBehaviours(List<BlockEntityBehaviour> behaviours) {

    }

    public void setVisualRotationalSpeed(double speed){
        rotationalSpeed = speed;
    }

    public void setProperty(double torqueRatio, double thrustRatio, boolean reverseTorque){
        this.TorqueRatio = torqueRatio;
        this.ThrustRatio = thrustRatio;
        this.ReverseTorque = reverseTorque;
        sendData();
    }


    @Override
    public void tick() {
        super.tick();
        targetAngle = Util.angleReset(targetAngle + (float) (rotationalSpeed * 0.05 * 0.175));
        if (level.isClientSide) {
            tickAnimation();
        }
    }

    @Override
    public void lazyTick() {
        if(level.isClientSide)return;
        syncClient();
    }

    @OnlyIn(Dist.CLIENT)
    private void tickAnimation(){
        angle.chase(targetAngle, 0.1, LerpedFloat.Chaser.EXP);
        angle.tickChaser();
    }


    @Override
    protected void write(CompoundTag tag, boolean clientPacket) {
        if(!clientPacket){
            tag.putDouble("ThrustRatio", ThrustRatio);
            tag.putDouble("TorqueRatio", TorqueRatio);
            tag.putBoolean("ReverseTorque", ReverseTorque);
        }
        super.write(tag, clientPacket);
    }

    @Override
    protected void read(CompoundTag tag, boolean clientPacket) {
        if(!clientPacket){
            ThrustRatio = tag.getDouble("ThrustRatio");
            TorqueRatio = tag.getDouble("TorqueRatio");
            ReverseTorque = tag.getBoolean("ReverseTorque");
        }
        super.read(tag, clientPacket);
    }

    @Override
    public void syncClient() {
        if(!level.isClientSide){
            var p = new PropellerSyncAnimationPacket(getBlockPos(), rotationalSpeed);
            AllPackets.getChannel().send(PacketDistributor.ALL.noArg(), p);
        }
    }

    public static class PropellerAnimationDataHandler extends PacketHandler {
        private double rotationalSpeed;

        public PropellerAnimationDataHandler(){
            rotationalSpeed = 0;
        }

        public PropellerAnimationDataHandler(double speed){
            rotationalSpeed = speed;
        }

        @Override
        public void readBuffer(FriendlyByteBuf buffer) {
            rotationalSpeed = buffer.readDouble();
        }

        @Override
        public void writeBuffer(FriendlyByteBuf buffer) {
            buffer.writeDouble(rotationalSpeed);
        }

        @Override
        public void handle(BlockEntity be) {
            ((PropellerBlockEntity) be).setVisualRotationalSpeed(rotationalSpeed);
        }
    }
}

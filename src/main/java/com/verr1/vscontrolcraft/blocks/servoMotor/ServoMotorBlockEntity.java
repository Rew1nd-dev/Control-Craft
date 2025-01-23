package com.verr1.vscontrolcraft.blocks.servoMotor;

import com.simibubi.create.content.equipment.goggles.IHaveGoggleInformation;
import com.simibubi.create.foundation.utility.Lang;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.vscontrolcraft.base.Servo.AbstractServoMotor;
import com.verr1.vscontrolcraft.base.Servo.ServoMotorSyncAnimationPacket;
import com.verr1.vscontrolcraft.compat.cctweaked.peripherals.ServoMotorPeripheral;
import com.verr1.vscontrolcraft.registry.AllPackets;
import com.verr1.vscontrolcraft.utils.Util;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.ChatFormatting;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.Component;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.network.PacketDistributor;
import org.jetbrains.annotations.NotNull;
import org.joml.*;

import javax.annotation.Nullable;
import java.util.List;

import static net.minecraft.ChatFormatting.GRAY;

public class ServoMotorBlockEntity extends AbstractServoMotor implements IHaveGoggleInformation {

    private boolean assembleNextTick = false;

    private final LerpedFloat animatedLerpedAngle = LerpedFloat.angular();
    private float animatedAngle = 0;


    public ServoMotorBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        lazyTickRate = 0;
    }

    @Override
    public boolean addToGoggleTooltip(List<Component> tooltip, boolean isPlayerSneaking) {
        Lang.translate("tooltip.stressImpact")
                .style(GRAY)
                .forGoggles(tooltip);

        float stressTotal = 0;

        Lang.number(stressTotal)
                .translate("generic.unit.stress")
                .style(ChatFormatting.AQUA)
                .space()
                .add(Lang.translate("gui.goggles.at_current_speed")
                        .style(ChatFormatting.DARK_GRAY))
                .forGoggles(tooltip, 1);
        return true;
    }

    public Vector3d getServoDirectionJOML(){
        return Util.Vec3itoVector3d(getBlockState().getValue(ServoMotorBlock.FACING).getNormal()) ;
    }

    @Override
    public BlockPos getAssembleBlockPos() {
        return getBlockPos().relative(getBlockState().getValue(ServoMotorBlock.FACING));
    }

    @Override
    public Vector3d getAssembleBlockPosJOML() {
        Vector3d center = Util.Vec3toVector3d(getAssembleBlockPos().getCenter());
        Vector3d dir = getServoDirectionJOML();
        return center.fma(0.0, dir);
    }

    public Direction getServoDirection(){
        return getBlockState().getValue(ServoMotorBlock.FACING);
    }

    public void setAssembleNextTick(){
        assembleNextTick = true;
    }

    @Override
    public void tick() {
        super.tick();
        if(assembleNextTick){
            assemble();
            assembleNextTick = false;
        }

        syncAssemAttachInducer();
        if(level.isClientSide){
            tickAnimation();
        }
    }

    public void tickAnimation(){
        animatedLerpedAngle.chase(animatedAngle, 0.5, LerpedFloat.Chaser.EXP);
        animatedLerpedAngle.tickChaser();
    }

    @Override
    public void lazyTick() {
        super.lazyTick();
        if(level.isClientSide)return;
        syncClient();
    }


    public void syncClient(){
        if(!level.isClientSide){
            var p = new ServoMotorSyncAnimationPacket(getBlockPos(), getServoAngle());
            AllPackets.getChannel().send(PacketDistributor.ALL.noArg(), p);
        }
    }

    public void setAnimatedAngle(double angle) {
        animatedAngle = (float)angle;
    }

    public float getAnimatedAngle(float partialTick) {
        return animatedLerpedAngle.getValue(partialTick);
    }


}

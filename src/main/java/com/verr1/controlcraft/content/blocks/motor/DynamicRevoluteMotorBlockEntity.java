package com.verr1.controlcraft.content.blocks.motor;

import com.simibubi.create.foundation.gui.ScreenOpener;
import com.verr1.controlcraft.content.gui.legacy.RevoluteMotorScreen;
import com.verr1.controlcraft.foundation.data.control.PID;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundClientPacket;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundServerPacket;
import com.verr1.controlcraft.foundation.type.descriptive.CheatMode;
import com.verr1.controlcraft.foundation.type.descriptive.LockMode;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.foundation.type.descriptive.TargetMode;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.network.NetworkEvent;
import org.joml.Vector3d;

public class DynamicRevoluteMotorBlockEntity extends AbstractDynamicMotor {
    public DynamicRevoluteMotorBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    @Override
    public Direction getServoDirection() {
        return getBlockState().getValue(DynamicRevoluteMotorBlock.FACING);
    }

    @Override
    public BlockPos getAssembleBlockPos() {
        return getBlockPos().relative(getBlockState().getValue(DynamicRevoluteMotorBlock.FACING));
    }

    @Override
    public Vector3d getRotationCenterPosJOML() {
        Vector3d center = ValkyrienSkies.set(new Vector3d(), getAssembleBlockPos().getCenter());
        return center.add(getOffset());
    }


    public void displayScreen(ServerPlayer player){

        double t = getController().getTarget();
        double v = getController().getValue();
        double o = getOffset().get(0);
        boolean m = getTargetMode() == TargetMode.POSITION;
        boolean c = getCheatMode() == CheatMode.NO_REPULSE;
        boolean l = isLocked();
        PID pidParams = getController().PID();

        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.OPEN_SCREEN_0)
                .withDouble(t)
                .withDouble(v)
                .withDouble(pidParams.p())
                .withDouble(pidParams.i())
                .withDouble(pidParams.d())
                .withDouble(o)
                .withBoolean(m)
                .withBoolean(c)
                .withBoolean(l)
                .build();

        ControlCraftPackets.sendToPlayer(p, player);
    }


    @Override
    public void handleServer(NetworkEvent.Context context, BlockBoundServerPacket packet) {
        if(packet.getType() == RegisteredPacketType.SETTING_0){
            // setOffset(new Vector3d(packet.getDoubles().get(0), 0, 0));
        }
        if(packet.getType() == RegisteredPacketType.TOGGLE_0){
            setCheatMode(cheatMode == CheatMode.NONE ? CheatMode.NO_REPULSE : CheatMode.NONE);
        }
        if(packet.getType() == RegisteredPacketType.TOGGLE_1){
            setReverseCreateInput(!reverseCreateInput);
        }
        if(packet.getType() == RegisteredPacketType.TOGGLE_2){
            setLockMode(lockMode == LockMode.ON ? LockMode.OFF : LockMode.ON);
        }
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void handleClient(NetworkEvent.Context context, BlockBoundClientPacket packet) {
        super.handleClient(context, packet);
        /*
        if(packet.getType() == RegisteredPacketType.SYNC_0){
            double angle = packet.getDoubles().get(0);
            clientAngle = (float) angle;
        }
        * */
        if(packet.getType() == RegisteredPacketType.OPEN_SCREEN_0){
            double t = packet.getDoubles().get(0);
            double v = packet.getDoubles().get(1);
            double p = packet.getDoubles().get(2);
            double i = packet.getDoubles().get(3);
            double d = packet.getDoubles().get(4);
            double o = packet.getDoubles().get(5);
            boolean m = packet.getBooleans().get(0);
            boolean c = packet.getBooleans().get(1);
            boolean l = packet.getBooleans().get(2);
            DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () ->
                    ScreenOpener.open(new RevoluteMotorScreen(getBlockPos(), p, i, d, v, t, o, m, c, l)));
        }
    }

}

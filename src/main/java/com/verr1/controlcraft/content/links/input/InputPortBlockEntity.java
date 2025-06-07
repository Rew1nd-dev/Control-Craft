package com.verr1.controlcraft.content.links.input;

import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.InputLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

public class InputPortBlockEntity extends CimulinkBlockEntity<InputLinkPort> implements
        IReceiver
{
    public static final NetworkKey INPUT = NetworkKey.create("link_input");


    private final DirectReceiver receiver = new DirectReceiver();

    public InputPortBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(INPUT)
                .withBasic(SerializePort.of(
                        () -> linkPort().peek(),
                        t -> linkPort().input(t),
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .register();

        buildRegistry(FIELD)
                .withBasic(CompoundTagPort.of(
                        () -> receiver().serialize(),
                        t -> receiver().deserialize(t)
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.UNIT, CompoundTag.class)
                )
                .dispatchToSync()
                .register();

        receiver().register(
                new NumericField(
                        () -> linkPort().peek(),
                        t -> linkPort().input(t),
                        "input"
                ),
                new DirectReceiver.InitContext(SlotType.INPUT, Couple.create(0.0, 1.0)),
                6
        );
    }

    @Override
    public void tickServer() {
        super.tickServer();
        linkPort().tick();
    }

    @Override
    protected InputLinkPort create(@NotNull Level level, BlockPos pos) {
        return new InputLinkPort(WorldBlockPos.of(level, pos));
    }

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }
}

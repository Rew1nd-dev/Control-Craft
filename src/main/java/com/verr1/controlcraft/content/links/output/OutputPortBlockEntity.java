package com.verr1.controlcraft.content.links.output;

import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.OutputLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import java.util.Arrays;

public class OutputPortBlockEntity extends CimulinkBlockEntity<OutputLinkPort> implements
        IReceiver
{

    public static final NetworkKey OUTPUT = NetworkKey.create("link_output");

    private final DirectReceiver receiver = new DirectReceiver();
    private int lastOutputSignal = 0;
    private boolean receivedSignalChanged = false;


    public OutputPortBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(OUTPUT)
                .withBasic(SerializePort.of(
                        () -> linkPort().peek(),
                        $ -> {},
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
                        () -> 1.0,
                        $ -> {},
                        "sensor"
                ),
                new DirectReceiver.InitContext(SlotType.OUTPUT, Couple.create(0.0, 1.0))
        );
    }


    public void updateOutputSignal(){
        if(level == null || level.isClientSide)return;
        // if(!fields.get(0).directionOptional.test(side))return 0;

        double d = linkPort().peek();

        double a = receiver().view().get(0).view().get(0).min_max.get(true);
        double b = receiver().view().get(0).view().get(0).min_max.get(false);
        double ratio = MathUtils.clampHalf(
                Math.abs(d - a) / (Math.abs(a - b) + 1e-8), 1
        );

        int newSignal = (int)(ratio * 15);
        if(newSignal != lastOutputSignal){
            receivedSignalChanged = true;
            lastOutputSignal = newSignal;
        }

    }

    public int getOutputSignal(){
        return lastOutputSignal;
    }

    public void updateNeighbor(){
        if(level == null)return;
        if (!receivedSignalChanged)return;
        receivedSignalChanged = false;
        Arrays
                .stream(Direction.values())
                .filter(receiver().view().get(0).view().get(0).direction::test)
                .forEach(
                        face -> {
                            BlockPos attachedPos = worldPosition.relative(face);
                            level.blockUpdated(worldPosition, level.getBlockState(worldPosition)
                                    .getBlock());
                            level.blockUpdated(attachedPos, level.getBlockState(attachedPos)
                                    .getBlock());
                        }
                );
    }

    @Override
    public void tickServer() {
        super.tickServer();
        linkPort().tick();
        updateOutputSignal();
        updateNeighbor();
    }

    @Override
    protected OutputLinkPort create() {
        return new OutputLinkPort();
    }

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }
}

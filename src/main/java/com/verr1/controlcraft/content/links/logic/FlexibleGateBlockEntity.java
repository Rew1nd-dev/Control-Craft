package com.verr1.controlcraft.content.links.logic;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.FlexibleGateLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.StringBooleans;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.registry.CimulinkBlockEntities;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class FlexibleGateBlockEntity extends CimulinkBlockEntity<FlexibleGateLinkPort> {

    public static final NetworkKey AND = NetworkKey.create("is_and");
    public static final NetworkKey OUTPUT_MASK = NetworkKey.create("output_mask");
    public static final NetworkKey STATUS = NetworkKey.create("flexible_gate_status");
    public static final NetworkKey ADD_PORT = NetworkKey.create("add_flexible_port");
    public static final NetworkKey REMOVE_PORT = NetworkKey.create("remove_flexible_port");

    public FlexibleGateBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        /*
        * buildRegistry(AND)
                .withBasic(SerializePort.of(
                        this::isAnd,
                        this::setAnd,
                        SerializeUtils.BOOLEAN
                ))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();
        * */
        buildRegistry(AND)
                .withBasic(SerializePort.of(
                        this::getGateType,
                        this::setGateType,
                        SerializeUtils.ofEnum(GateTypes.class)
                ))
                .withClient(ClientBuffer.of(GateTypes.class))
                .register();

        buildRegistry(OUTPUT_MASK)
                .withBasic(SerializePort.of(
                        () -> linkPort().outputMask(),
                        a -> linkPort().setOutputMask(a),
                        SerializeUtils.BOOLEAN
                ))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        buildRegistry(STATUS)
                .withBasic(SerializePort.of(
                        () -> linkPort().getMask(),
                        a -> linkPort().setMask(a),
                        StringBooleans.SERIALIZER
                ))
                .withClient(new ClientBuffer<>(
                        StringBooleans.SERIALIZER,
                        StringBooleans.class
                ))
                .register();

        panel().registerUnit(ADD_PORT, this::addPort);
        panel().registerUnit(REMOVE_PORT, this::removePort);

    }

    public void setAnd(boolean isAnd){
        linkPort().setAndGate(isAnd);
        MinecraftUtils.updateBlockState(
                getLevel(),
                getBlockPos(),
                getBlockState().setValue(FlexibleGateBlock.IS_AND, isAnd)
        );
    }

    public boolean isAnd(){
        return linkPort().isAndGate();
    }

    public GateTypes getGateType() {
        return linkPort().isAndGate() ? GateTypes.AND : GateTypes.OR;
    }

    public void setGateType(GateTypes type){
        if(type == GateTypes.AND){
            setAnd(true);
        } else if(type == GateTypes.OR){
            setAnd(false);
        }
    }


    public void addPort(){
        linkPort().setSize(linkPort().size() + 1);
    }

    public void removePort(){
        linkPort().setSize(linkPort().size() - 1);
    }

    @Override
    protected FlexibleGateLinkPort create() {
        return new FlexibleGateLinkPort();
    }


}








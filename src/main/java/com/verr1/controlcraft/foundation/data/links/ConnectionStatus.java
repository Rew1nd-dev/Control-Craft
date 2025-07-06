package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.Level;

import java.util.*;

public class ConnectionStatus {
    public static final ConnectionStatus EMPTY = new ConnectionStatus();

    public static final Serializer<List<String>> NAMES =
            SerializeUtils.ofList(SerializeUtils.STRING);



    public final Map<String, Set<BlockPort>> outputPorts = new HashMap<>();
    public final Map<String, BlockPort> inputPorts = new HashMap<>();

    public final List<String> inputs = new ArrayList<>();
    public final List<String> outputs = new ArrayList<>();

    public String clazzName;


    public ConnectionStatus(){}

    public ConnectionStatus(
            Map<String, Set<BlockPort>> outputPorts,
            Map<String, BlockPort> inputPorts,
            List<String> inputs,
            List<String> outputs,
            String identifier
    ) {
        this.outputPorts.putAll(outputPorts);
        this.inputPorts.putAll(inputPorts);
        this.inputs.addAll(inputs);
        this.outputs.addAll(outputs);



        this.clazzName = identifier;
    }

    public String in(int index){
        if(index >= inputs.size() || index < 0)return "out of bound";
        return inputs.get(index);
    }

    public String out(int index){
        if(index >= outputs.size() || index < 0)return "out of bound";
        return outputs.get(index);
    }


    public static String mapToName(BlockPos pos, Level level){
        return BlockEntityGetter.getLevelBlockEntityAt(level, pos, CimulinkBlockEntity.class)
                .map(CimulinkBlockEntity::readClientComponentName)
                .orElse("not found");
    }

    public static CompoundTag summarize(BlockLinkPort blp){
        return CompoundTagBuilder.create()
                .withCompound("forward", blp.serializeForward())
                .withCompound("backward", blp.serializeBackward())
                .withCompound("inputs", NAMES.serialize(blp.inputsNamesExcludeSignals()))
                .withCompound("outputs", NAMES.serialize(blp.outputsNames()))
                .withCompound("clazz", SerializeUtils.STRING.serialize(blp.getClass().getSimpleName()))
                .build();
    }

    public static ConnectionStatus deserialize(CompoundTag tag){
        return new ConnectionStatus(
                BlockLinkPort.deserializeForward(tag.getCompound("forward")),
                BlockLinkPort.deserializeBackward(tag.getCompound("backward")),
                NAMES.deserialize(tag.getCompound("inputs")),
                NAMES.deserialize(tag.getCompound("outputs")),
                SerializeUtils.STRING.deserialize(tag.getCompound("clazz"))
        );
    }


}

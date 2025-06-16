package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.InputLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.OutputLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class CircuitTagBuilder {
    Set<String> existName = new HashSet<>();

    Map<WorldBlockPos, BlockLinkPort> normals  = new HashMap<>();
    Map<WorldBlockPos, BlockLinkPort> inputs   = new HashMap<>();
    Map<WorldBlockPos, BlockLinkPort> outputs = new HashMap<>();

    List<ComponentNbt> componentSummaries;
    List<ConnectionNbt> connectionNbts;
    List<IoNbt> inOuts;

    public static CircuitTagBuilder of(List<BlockLinkPort> blps){
        return new CircuitTagBuilder(blps);
    }

    public CircuitTagBuilder(List<BlockLinkPort> blps){
        blps.forEach(blp -> {
            if(blp instanceof ISummarizable){
                normals.put(blp.pos(), convertName(blp));
            } else if (blp instanceof InputLinkPort input) {
                inputs.put(blp.pos(), convertName(input));
            } else if (blp instanceof OutputLinkPort output) {
                outputs.put(output.pos(), convertName(output));
            }
        });

        ArrayUtils.AssertDifferent(normals.keySet(), inputs.keySet());
        ArrayUtils.AssertDifferent(outputs.keySet(), inputs.keySet());
        ArrayUtils.AssertDifferent(normals.keySet(), outputs.keySet());
    }

    public static @NotNull CircuitNbt compile(ServerLevel level, BlockPos ul, BlockPos lr) throws IllegalArgumentException{
        int x1 = Math.min(ul.getX(), lr.getX());
        int y1 = Math.min(ul.getY(), lr.getY());
        int z1 = Math.min(ul.getZ(), lr.getZ());
        int x2 = Math.max(ul.getX(), lr.getX());
        int y2 = Math.max(ul.getY(), lr.getY());
        int z2 = Math.max(ul.getZ(), lr.getZ());
        List<BlockLinkPort> blps = new ArrayList<>();
        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {

                    WorldBlockPos wbp = WorldBlockPos.of(level, new BlockPos(x, y, z));
                    BlockLinkPort.of(wbp).ifPresent(blps::add);
                }
            }
        }

        return CircuitTagBuilder.of(blps).buildNbt();
    }

    public CircuitNbt buildNbt() throws IllegalArgumentException{
        connectionNbts = new ArrayList<>();
        inOuts = new ArrayList<>();
        componentSummaries = new ArrayList<>();
        normals.forEach((pos, blp) -> {
            String inName = blp.name();

            componentSummaries.add(new ComponentNbt(inName, ((ISummarizable)blp).summary()));

            blp.backwardLinks().forEach((inPortName, bp) -> {
                String outPortName = bp.portName();
                if(normals.containsKey(bp.pos())){
                    String outName = normals.get(bp.pos()).name();
                    connectionNbts.add(new ConnectionNbt(
                            outName,
                            outPortName,
                            inName,
                            inPortName
                    ));
                }else if(inputs.containsKey(bp.pos())){
                    String circuitInName = inputs.get(bp.pos()).name();
                    inOuts.add(new IoNbt(
                            true,
                            circuitInName,
                            inName,
                            inPortName
                    ));
                }else {
                    throw new IllegalArgumentException("BlockLinkPort: " + bp + " is not connected to any port!");
                }
            });
        });

        /*
        else if (outputs.containsKey(bp.pos())) {
                    // actually, should not happen
                    String circuitOutName = outputs.get(bp.pos()).name();
                    inOuts.add(new IoNbt(
                            false,
                            circuitOutName,
                            inName,
                            inPortName
                    ));
                }
        * */

        outputs.forEach((pos, blp) -> {
            String outputName = blp.name();
            blp.backwardLinks().forEach((inName, bp) -> {
                if(normals.containsKey(bp.pos())){
                    String outPortName = bp.portName();
                    String outName = normals.get(bp.pos()).name();
                    inOuts.add(new IoNbt(
                            false,
                            outputName,
                            outName,
                            outPortName
                    ));
                }else if(inputs.containsKey(bp.pos())){
                    throw new IllegalArgumentException("OutputLinkPort: " + bp + " is connected by input directly! Which makes no sense!");
                }
            });
        });

        return new CircuitNbt(
                componentSummaries,
                connectionNbts,
                inOuts
        );
    }



    public BlockLinkPort convertName(BlockLinkPort blp){
        if(existName.contains(blp.name())){

            int count = 1;
            while(existName.contains(blp.name() + "(" + count + ")")){
                count++;
            }
            blp.setName(blp.name() + "(" + count + ")");

        }

        existName.add(blp.name());
        return blp;
    }


}

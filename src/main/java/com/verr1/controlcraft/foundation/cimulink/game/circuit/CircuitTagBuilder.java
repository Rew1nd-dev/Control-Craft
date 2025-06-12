package com.verr1.controlcraft.foundation.cimulink.game.circuit;

import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ISummarizable;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.InputLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.OutputLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;

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

    public CircuitNbt buildNbt(){
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

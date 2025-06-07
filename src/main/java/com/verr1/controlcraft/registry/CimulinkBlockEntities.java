package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.content.links.comparator.ComparatorBlock;
import com.verr1.controlcraft.content.links.comparator.ComparatorBlockEntity;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.ff.FFBlockEntity;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlock;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlockEntity;
import com.verr1.controlcraft.content.links.input.InputPortBlock;
import com.verr1.controlcraft.content.links.input.InputPortBlockEntity;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;
import com.verr1.controlcraft.content.links.mux2.Mux2Block;
import com.verr1.controlcraft.content.links.mux2.Mux2BlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlock;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlock;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlockEntity;

import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class CimulinkBlockEntities {

    public static final BlockEntityEntry<LogicGateBlockEntity> LOGIC_GATE_BLOCKENTITY = REGISTRATE
            .blockEntity(LogicGateBlock.ID, LogicGateBlockEntity::new)
            .validBlock(CimulinkBlocks.LOGIC_GATE)
            .register();

    public static final BlockEntityEntry<FFBlockEntity> FF_BLOCKENTITY = REGISTRATE
            .blockEntity(FFBlock.ID, FFBlockEntity::new)
            .validBlock(CimulinkBlocks.FF)
            .register();

    public static final BlockEntityEntry<InputPortBlockEntity> INPUT_BLOCKENTITY = REGISTRATE
            .blockEntity(InputPortBlock.ID, InputPortBlockEntity::new)
            .validBlock(CimulinkBlocks.INPUT)
            .register();

    public static final BlockEntityEntry<OutputPortBlockEntity> OUTPUT_BLOCKENTITY = REGISTRATE
            .blockEntity(OutputPortBlock.ID, OutputPortBlockEntity::new)
            .validBlock(CimulinkBlocks.OUTPUT)
            .register();

    public static final BlockEntityEntry<ShifterLinkBlockEntity> SHIFTER_BLOCKENTITY = REGISTRATE
            .blockEntity(ShifterLinkBlock.ID, ShifterLinkBlockEntity::new)
            .validBlock(CimulinkBlocks.SHIFTER)
            .register();

    public static final BlockEntityEntry<LinearAdderBlockEntity> FMA_BLOCKENTITY = REGISTRATE
            .blockEntity(LinearAdderBlock.ID, LinearAdderBlockEntity::new)
            .validBlock(CimulinkBlocks.FMA)
            .register();

    public static final BlockEntityEntry<Mux2BlockEntity> MUX_BLOCKENTITY = REGISTRATE
            .blockEntity(Mux2Block.ID, Mux2BlockEntity::new)
            .validBlock(CimulinkBlocks.MUX)
            .register();

    public static final BlockEntityEntry<ComparatorBlockEntity> COMPARATOR_BLOCKENTITY = REGISTRATE
            .blockEntity(ComparatorBlock.ID, ComparatorBlockEntity::new)
            .validBlock(CimulinkBlocks.COMPARATOR)
            .register();

    public static void register(){}
}

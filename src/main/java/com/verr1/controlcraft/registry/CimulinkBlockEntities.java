package com.verr1.controlcraft.registry;

import com.tterrag.registrate.util.entry.BlockEntityEntry;
import com.verr1.controlcraft.content.links.circuit.CircuitBlock;
import com.verr1.controlcraft.content.links.circuit.CircuitBlockEntity;
import com.verr1.controlcraft.content.links.comparator.ComparatorBlock;
import com.verr1.controlcraft.content.links.comparator.ComparatorBlockEntity;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.ff.FFBlockEntity;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlock;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlockEntity;
import com.verr1.controlcraft.content.links.func.FunctionsBlock;
import com.verr1.controlcraft.content.links.func.FunctionsBlockEntity;
import com.verr1.controlcraft.content.links.input.InputPortBlock;
import com.verr1.controlcraft.content.links.input.InputPortBlockEntity;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;
import com.verr1.controlcraft.content.links.mux2.Mux2Block;
import com.verr1.controlcraft.content.links.mux2.Mux2BlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlock;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlock;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlockEntity;
import com.verr1.controlcraft.content.links.sensor.SensorBlock;
import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlock;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlockEntity;
import com.verr1.controlcraft.content.links.signal.DirectCurrentBlock;
import com.verr1.controlcraft.content.links.signal.DirectCurrentBlockEntity;
import com.verr1.controlcraft.render.CimulinkSocketRenderer;

import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class CimulinkBlockEntities {

    public static final BlockEntityEntry<LogicGateBlockEntity> LOGIC_GATE_BLOCKENTITY = REGISTRATE
            .blockEntity(LogicGateBlock.ID, LogicGateBlockEntity::new)
            .validBlock(CimulinkBlocks.LOGIC_GATE)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<FFBlockEntity> FF_BLOCKENTITY = REGISTRATE
            .blockEntity(FFBlock.ID, FFBlockEntity::new)
            .validBlock(CimulinkBlocks.FF)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<InputPortBlockEntity> INPUT_BLOCKENTITY = REGISTRATE
            .blockEntity(InputPortBlock.ID, InputPortBlockEntity::new)
            .validBlock(CimulinkBlocks.INPUT)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<OutputPortBlockEntity> OUTPUT_BLOCKENTITY = REGISTRATE
            .blockEntity(OutputPortBlock.ID, OutputPortBlockEntity::new)
            .validBlock(CimulinkBlocks.OUTPUT)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<ShifterLinkBlockEntity> SHIFTER_BLOCKENTITY = REGISTRATE
            .blockEntity(ShifterLinkBlock.ID, ShifterLinkBlockEntity::new)
            .validBlock(CimulinkBlocks.SHIFTER)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<LinearAdderBlockEntity> FMA_BLOCKENTITY = REGISTRATE
            .blockEntity(LinearAdderBlock.ID, LinearAdderBlockEntity::new)
            .validBlock(CimulinkBlocks.FMA)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<Mux2BlockEntity> MUX_BLOCKENTITY = REGISTRATE
            .blockEntity(Mux2Block.ID, Mux2BlockEntity::new)
            .validBlock(CimulinkBlocks.MUX)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<ComparatorBlockEntity> COMPARATOR_BLOCKENTITY = REGISTRATE
            .blockEntity(ComparatorBlock.ID, ComparatorBlockEntity::new)
            .validBlock(CimulinkBlocks.COMPARATOR)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<ProxyLinkBlockEntity> PROXY_BLOCKENTITY = REGISTRATE
            .blockEntity(ProxyLinkBlock.ID, ProxyLinkBlockEntity::new)
            .validBlock(CimulinkBlocks.PROXY)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<CircuitBlockEntity> CIRCUIT_BLOCKENTITY = REGISTRATE
            .blockEntity(CircuitBlock.ID, CircuitBlockEntity::new)
            .validBlock(CimulinkBlocks.CIRCUIT)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<DirectCurrentBlockEntity> DC_BLOCKENTITY = REGISTRATE
            .blockEntity(DirectCurrentBlock.ID, DirectCurrentBlockEntity::new)
            .validBlock(CimulinkBlocks.DC)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<FunctionsBlockEntity> FUNCTIONS_BLOCKENTITY = REGISTRATE
            .blockEntity(FunctionsBlock.ID, FunctionsBlockEntity::new)
            .validBlock(CimulinkBlocks.FUNCTIONS)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static final BlockEntityEntry<SensorBlockEntity> SENSOR_BLOCKENTITY = REGISTRATE
            .blockEntity(SensorBlock.ID, SensorBlockEntity::new)
            .validBlock(CimulinkBlocks.SENSOR)
            .renderer(() -> CimulinkSocketRenderer::new)
            .register();

    public static void register(){}
}

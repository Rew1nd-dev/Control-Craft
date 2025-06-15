package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.BlockStateGen;
import com.simibubi.create.foundation.data.SharedProperties;
import com.simibubi.create.foundation.data.TagGen;
import com.tterrag.registrate.util.entry.BlockEntry;
import com.verr1.controlcraft.content.blocks.loader.ChunkLoaderBlock;
import com.verr1.controlcraft.content.links.circuit.CircuitBlock;
import com.verr1.controlcraft.content.links.comparator.ComparatorBlock;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlock;
import com.verr1.controlcraft.content.links.func.FunctionsBlock;
import com.verr1.controlcraft.content.links.input.InputPortBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.content.links.mux2.Mux2Block;
import com.verr1.controlcraft.content.links.output.OutputPortBlock;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlock;
import com.verr1.controlcraft.content.links.sensor.SensorBlock;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlock;
import com.verr1.controlcraft.content.links.signal.DirectCurrentBlock;
import net.minecraft.world.level.material.MapColor;

import static com.simibubi.create.foundation.data.ModelGen.customItemModel;
import static com.verr1.controlcraft.ControlCraft.REGISTRATE;

public class CimulinkBlocks {

    static {
        REGISTRATE.setCreativeTab(ControlCraftCreativeTabs.CIMULINK);
    }

    public static final BlockEntry<LogicGateBlock> LOGIC_GATE = REGISTRATE
            .block(LogicGateBlock.ID, LogicGateBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    LogicGateBlock.GateDataGenerator.generate()
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<FFBlock> FF = REGISTRATE
            .block(FFBlock.ID, FFBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    FFBlock.FFDataGenerator.generate()
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<InputPortBlock> INPUT = REGISTRATE
            .block(InputPortBlock.ID, InputPortBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<OutputPortBlock> OUTPUT = REGISTRATE
            .block(OutputPortBlock.ID, OutputPortBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<ShifterLinkBlock> SHIFTER = REGISTRATE
            .block(ShifterLinkBlock.ID, ShifterLinkBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<LinearAdderBlock> FMA = REGISTRATE
            .block(LinearAdderBlock.ID, LinearAdderBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<Mux2Block> MUX = REGISTRATE
            .block(Mux2Block.ID, Mux2Block::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<ComparatorBlock> COMPARATOR = REGISTRATE
            .block(ComparatorBlock.ID, ComparatorBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<ProxyLinkBlock> PROXY = REGISTRATE
            .block(ProxyLinkBlock.ID, ProxyLinkBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<CircuitBlock> CIRCUIT = REGISTRATE
            .block(CircuitBlock.ID, CircuitBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<DirectCurrentBlock> DC = REGISTRATE
            .block(DirectCurrentBlock.ID, DirectCurrentBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<FunctionsBlock> FUNCTIONS = REGISTRATE
            .block(FunctionsBlock.ID, FunctionsBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static final BlockEntry<SensorBlock> SENSOR = REGISTRATE
            .block(SensorBlock.ID, SensorBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .register();

    public static void register(){}
}

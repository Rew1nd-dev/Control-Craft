package com.verr1.controlcraft.registry;

import com.simibubi.create.foundation.data.BlockStateGen;
import com.simibubi.create.foundation.data.SharedProperties;
import com.simibubi.create.foundation.data.TagGen;
import com.tterrag.registrate.util.entry.BlockEntry;
import com.verr1.controlcraft.content.links.bus.BusBlock;
import com.verr1.controlcraft.content.links.ccbridge.CCBridgeBlock;
import com.verr1.controlcraft.content.links.integration.CircuitBlock;
import com.verr1.controlcraft.content.links.comparator.ComparatorBlock;
import com.verr1.controlcraft.content.links.connector.EasyConnectorBlock;
import com.verr1.controlcraft.content.links.ff.FFBlock;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlock;
import com.verr1.controlcraft.content.links.func.FunctionsBlock;
import com.verr1.controlcraft.content.links.input.InputPortBlock;
import com.verr1.controlcraft.content.links.integration.LuaBlock;
import com.verr1.controlcraft.content.links.logic.FlexibleGateBlock;
import com.verr1.controlcraft.content.links.logic.LogicGateBlock;
import com.verr1.controlcraft.content.links.mux2.Mux2Block;
import com.verr1.controlcraft.content.links.output.OutputPortBlock;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlock;
import com.verr1.controlcraft.content.links.scope.OscilloscopeBlock;
import com.verr1.controlcraft.content.links.sensor.SensorBlock;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlock;
import com.verr1.controlcraft.content.links.signal.DirectCurrentBlock;
import com.verr1.controlcraft.content.links.tweakerminal.TweakerminalBlock;
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
            .lang("Logic Gates")
            .register();

    public static final BlockEntry<FlexibleGateBlock> FLEXIBLE_GATE = REGISTRATE
            .block(FlexibleGateBlock.ID, FlexibleGateBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    FlexibleGateBlock.GateDataGenerator.generate()
            )
            .item()

            .transform(customItemModel())
            .lang("Flexible Logic Gates")
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
            .lang("Flip Flops")
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
            .lang("Input Port")
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
            .lang("Output Port")
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
            .lang("Shift Register")
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
            .lang("Linear Adder")
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
            .lang("2-1 Multiplexer")
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
            .lang("Comparator")
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
            .lang("Plant Interface")
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
            .lang("Uncompiled Circuit")
            .register();

    public static final BlockEntry<LuaBlock> LUA = REGISTRATE
            .block(LuaBlock.ID, LuaBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()

            .transform(customItemModel())
            .lang("Uncompiled Luacuit")
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
            .lang("Const Source")
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
            .lang("Functions")
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
            .lang("Inertial Measurement Unit")
            .register();

    public static final BlockEntry<CCBridgeBlock> CC_BRIDGE = REGISTRATE
            .block(CCBridgeBlock.ID, CCBridgeBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .lang("Computer Controlled Source")
            .register();

    public static final BlockEntry<BusBlock> BUS = REGISTRATE
            .block(BusBlock.ID, BusBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()
            .transform(customItemModel())
            .lang("Plant Bus")
            .register();

    public static final BlockEntry<OscilloscopeBlock> SCOPE = REGISTRATE
            .block(OscilloscopeBlock.ID, OscilloscopeBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()

            .transform(customItemModel())
            .lang("Oscilloscope")
            .register();

    public static final BlockEntry<TweakerminalBlock> TWEAKERMINAL = REGISTRATE
            .block(TweakerminalBlock.ID, TweakerminalBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()

            .transform(customItemModel())
            .lang("Tweakerminal")
            .register();

    public static final BlockEntry<EasyConnectorBlock> CONNECTOR = REGISTRATE
            .block(EasyConnectorBlock.ID, EasyConnectorBlock::new)
            .initialProperties(SharedProperties::stone)
            .properties(p -> p.noOcclusion().mapColor(MapColor.PODZOL))
            .transform(TagGen.axeOrPickaxe())
            .blockstate(
                    BlockStateGen.directionalBlockProvider(true)
            )
            .item()

            .transform(customItemModel())
            .lang("Easy Connector")
            .register();



    public static void register(){}
}

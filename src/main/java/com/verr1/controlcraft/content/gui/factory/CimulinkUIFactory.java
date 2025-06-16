package com.verr1.controlcraft.content.gui.factory;

import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.content.gui.layouts.api.LabelProvider;
import com.verr1.controlcraft.content.gui.layouts.element.*;
import com.verr1.controlcraft.content.gui.layouts.element.general.*;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.content.links.ff.FFBlockEntity;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlockEntity;
import com.verr1.controlcraft.content.links.func.FunctionsBlockEntity;
import com.verr1.controlcraft.content.links.input.InputPortBlockEntity;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlockEntity;
import com.verr1.controlcraft.content.links.shifter.ShifterLinkBlockEntity;
import com.verr1.controlcraft.content.links.signal.DirectCurrentBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.AnalogTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import net.minecraft.core.BlockPos;

import static com.verr1.controlcraft.content.gui.factory.Converter.alignLabel;
import static com.verr1.controlcraft.content.gui.factory.Converter.convert;
import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.*;

public class CimulinkUIFactory {


    public static GenericSettingScreen createGateScreen(BlockPos boundPos){

        OptionUIField<GateTypes> type = new OptionUIField<>(
                boundPos,
                LogicGateBlockEntity.GATE_TYPE,
                GateTypes.class,
                convert(UIContents.GATE_TYPES, Converter::titleStyle)
        );

        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );



        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, type)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createInputScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );

        DoubleUIField input = new DoubleUIField(
                boundPos,
                InputPortBlockEntity.INPUT,
                convert(UIContents.LINK_INPUT, Converter::titleStyle)
        );

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, input)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .build();
    }

    public static GenericSettingScreen createOutputScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );

        DoubleUIView input = new DoubleUIView(
                boundPos,
                OutputPortBlockEntity.OUTPUT,
                convert(UIContents.LINK_OUTPUT, Converter::titleStyle)
        );

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, input)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTickTask(createSyncTasks(boundPos, OutputPortBlockEntity.OUTPUT))
                .build();
    }

    public static GenericSettingScreen createFFScreen(BlockPos boundPos){

        OptionUIField<FFTypes> type = new OptionUIField<>(
                boundPos,
                FFBlockEntity.FF_TYPE,
                FFTypes.class,
                convert(UIContents.FF_TYPES, Converter::titleStyle)
        );

        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );



        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, type)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createFunctionsScreen(BlockPos boundPos){

        OptionUIField<AnalogTypes> type = new OptionUIField<>(
                boundPos,
                FunctionsBlockEntity.FUNCTIONS,
                AnalogTypes.class,
                convert(UIContents.FF_TYPES, Converter::titleStyle)
        );

        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );



        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, type)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createShifterScreen(BlockPos boundPos){

        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                title(UIContents.NAME)
        );

        LongUIField delay = new LongUIField(
                boundPos,
                ShifterLinkBlockEntity.DELAY,
                title(UIContents.SHIFTER_DELAY)
        );

        LongUIField parallel = new LongUIField(
                boundPos,
                ShifterLinkBlockEntity.PARALLEL,
                title(UIContents.SHIFTER_PARALLEL)
        );

        BooleanUIField async = new BooleanUIField(
                boundPos,
                ShifterLinkBlockEntity.ASYNC,
                title(UIContents.ASYNC_COMPONENT)
        );

        Runnable alignLabels = () -> alignLabel(delay, parallel, async);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, delay, parallel, async)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createFMAScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                title(UIContents.NAME)
        );

        CoeffUIPort coeffs = new CoeffUIPort(boundPos);

        UnitUIPanel inc = new UnitUIPanel(
                boundPos,
                LinearAdderBlockEntity.INC,
                Double.class,
                0.0,
                Converter.convert(UIContents.FMA_INC, Converter::titleStyle)
        );

        UnitUIPanel dec = new UnitUIPanel(
                boundPos,
                LinearAdderBlockEntity.DEC,
                Double.class,
                0.0,
                Converter.convert(UIContents.FMA_DEC, Converter::titleStyle)
        );

        Runnable alignLabels = () -> alignLabel(inc, dec);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, coeffs)
                                .withPreDoLayout(coeffs::alignLabels)
                                .build()
                )
                .withTab(
                        REMOTE_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(inc, dec)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createCircuitScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                title(UIContents.NAME)
        );

        CircuitUIPort circuit = new CircuitUIPort(boundPos);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.CIRCUIT.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, circuit)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createNameOnlyScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                title(UIContents.NAME)
        );

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createProxyScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                title(UIContents.NAME)
        );

        PortStatusUIPort ports = new PortStatusUIPort(boundPos);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, ports)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createDCScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );

        DoubleUIField input = new DoubleUIField(
                boundPos,
                DirectCurrentBlockEntity.DC,
                convert(UIContents.LINK_INPUT, Converter::titleStyle)
        );

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name, input)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createSensorScreen(BlockPos boundPos){
        StringUIField name = new StringUIField(
                boundPos,
                SharedKeys.COMPONENT_NAME,
                convert(UIContents.NAME, Converter::titleStyle)
        );

        SensorUIPort sensor = new SensorUIPort(boundPos);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(CimulinkBlocks.LOGIC_GATE.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(name)
                                .withPort(sensor)
                                .build()
                )
                .build();
    }

    public static LabelProvider title(Descriptive<?> d){
        return convert(d, Converter::titleStyle);
    }

}

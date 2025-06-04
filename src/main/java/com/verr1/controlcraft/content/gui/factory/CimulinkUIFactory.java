package com.verr1.controlcraft.content.gui.factory;

import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.element.DoubleUIField;
import com.verr1.controlcraft.content.gui.layouts.element.DoubleUIView;
import com.verr1.controlcraft.content.gui.layouts.element.OptionUIField;
import com.verr1.controlcraft.content.gui.layouts.element.StringUIField;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.content.links.ff.FFBlockEntity;
import com.verr1.controlcraft.content.links.input.InputPortBlockEntity;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;
import com.verr1.controlcraft.content.links.output.OutputPortBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import net.minecraft.core.BlockPos;

import static com.verr1.controlcraft.content.blocks.flap.FlapBearingBlockEntity.ANGLE;
import static com.verr1.controlcraft.content.gui.factory.Converter.convert;
import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.GENERIC_SETTING_TAB;
import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.createSyncTasks;

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
                                .withPort(input)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createOutputScreen(BlockPos boundPos){

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
                                .withPort(input)
                                .build()
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

}

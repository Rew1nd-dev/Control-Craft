package com.verr1.controlcraft.content.gui.factory;

import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.element.OptionUIField;
import com.verr1.controlcraft.content.gui.layouts.element.StringUIField;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.content.links.logic.LogicGateBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import com.verr1.controlcraft.registry.ControlCraftBlocks;
import net.minecraft.core.BlockPos;

import static com.verr1.controlcraft.content.gui.factory.Converter.convert;
import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.GENERIC_SETTING_TAB;
import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.REDSTONE_TAB;

public class CimulinkUIFactory {


    public static GenericSettingScreen createLogicGateScreen(BlockPos boundPos){



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

}

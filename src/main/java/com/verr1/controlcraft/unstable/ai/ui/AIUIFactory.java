package com.verr1.controlcraft.unstable.ai.ui;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.element.general.DoubleUIField;
import com.verr1.controlcraft.content.gui.layouts.element.general.StringUIField;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.unstable.blocks.cruiser.CruiserBlockEntity;
import net.minecraft.core.BlockPos;

public class AIUIFactory {

    public static GenericSettingScreen createCruiserScreen(BlockPos pos){

        StringUIField tar = new StringUIField(
                pos,
                CruiserBlockEntity.TAR,
                UIContents.AI_TAR.convertTo(Converter::titleStyle)
        );

        DoubleUIField vel = new DoubleUIField(
                pos,
                CruiserBlockEntity.VEL,
                UIContents.AI_VEL.convertTo(Converter::titleStyle)
        );

        DoubleUIField twi = new DoubleUIField(
                pos,
                CruiserBlockEntity.TWI,
                UIContents.AI_TWI.convertTo(Converter::titleStyle)
        );

        DoubleUIField tol = new DoubleUIField(
                pos,
                CruiserBlockEntity.TOL,
                UIContents.AI_TOL.convertTo(Converter::titleStyle)
        );

        DoubleUIField rad = new DoubleUIField(
                pos,
                CruiserBlockEntity.RAD,
                UIContents.AI_RAD.convertTo(Converter::titleStyle)
        );

        Runnable alignLabels = () -> Converter.alignLabel(tar, vel, tol, rad, twi);

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(tar, vel, tol, rad, twi)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }

}

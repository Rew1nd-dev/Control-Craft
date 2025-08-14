package com.verr1.controlcraft.unstable.ai.ui;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.element.general.BooleanUIField;
import com.verr1.controlcraft.content.gui.layouts.element.general.DoubleUIField;
import com.verr1.controlcraft.content.gui.layouts.element.general.StringUIField;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.blocks.jet.AiJetBlockEntity;
import net.minecraft.core.BlockPos;

public class AIUIFactory {

    public static GenericSettingScreen createCruiserScreen(BlockPos pos){

        StringUIField tar = new StringUIField(
                pos,
                SharedAIKeys.TAR,
                UIContents.AI_TAR.convertTo(Converter::titleStyle)
        );

        DoubleUIField vel = new DoubleUIField(
                pos,
                SharedAIKeys.VEL,
                UIContents.AI_VEL.convertTo(Converter::titleStyle)
        );

        DoubleUIField twi = new DoubleUIField(
                pos,
                SharedAIKeys.TWI,
                UIContents.AI_TWI.convertTo(Converter::titleStyle)
        );

        DoubleUIField tol = new DoubleUIField(
                pos,
                SharedAIKeys.TOL,
                UIContents.AI_TOL.convertTo(Converter::titleStyle)
        );

        DoubleUIField rad = new DoubleUIField(
                pos,
                SharedAIKeys.RAD,
                UIContents.AI_RAD.convertTo(Converter::titleStyle)
        );

        DoubleUIField yaw = new DoubleUIField(
                pos,
                SharedAIKeys.YAW,
                UIContents.AI_YAW.convertTo(Converter::titleStyle)
        );

        BooleanUIField flight = new BooleanUIField(
                pos,
                SharedAIKeys.ACTUAL_FLIGHT,
                UIContents.AI_FLIGHT.convertTo(Converter::titleStyle)
        );

        BooleanUIField weapon = new BooleanUIField(
                pos,
                SharedAIKeys.ACTUAL_WEAPON,
                UIContents.AI_WEAPON.convertTo(Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(tar, vel, tol, rad, twi, yaw);
            Converter.alignLabel(flight, weapon);
        };



        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(tar, vel, tol, rad, twi, yaw, flight, weapon)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createJetScreen(BlockPos pos){

        StringUIField tar = new StringUIField(
                pos,
                AiJetBlockEntity.TAR,
                UIContents.AI_TAR.convertTo(Converter::titleStyle)
        );

        DoubleUIField vel = new DoubleUIField(
                pos,
                AiJetBlockEntity.VEL,
                UIContents.AI_VEL.convertTo(Converter::titleStyle)
        );

        DoubleUIField twi = new DoubleUIField(
                pos,
                AiJetBlockEntity.TWI,
                UIContents.AI_TWI.convertTo(Converter::titleStyle)
        );

        DoubleUIField tol = new DoubleUIField(
                pos,
                AiJetBlockEntity.TOL,
                UIContents.AI_TOL.convertTo(Converter::titleStyle)
        );

        DoubleUIField rad = new DoubleUIField(
                pos,
                AiJetBlockEntity.RAD,
                UIContents.AI_RAD.convertTo(Converter::titleStyle)
        );

        DoubleUIField yaw = new DoubleUIField(
                pos,
                SharedAIKeys.YAW,
                UIContents.AI_YAW.convertTo(Converter::titleStyle)
        );



        Runnable alignLabels = () -> Converter.alignLabel(tar, vel, tol, rad, twi, yaw);

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(tar, vel, tol, rad, twi, yaw)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }


}

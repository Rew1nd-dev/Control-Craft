package com.verr1.controlcraft.unstable.ai.ui;

import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.element.general.*;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.blocks.schematic.SchematicBlockEntity;
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

        DoubleUIField e_rad = new DoubleUIField(
                pos,
                SharedAIKeys.E_RAD,
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

        // -------controller setting--------- //

        DoubleUIField p_common = new DoubleUIField(
                pos,
                SharedAIKeys.P_COMMON,
                UIContents.AI_P_COMMON.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_pitch = new DoubleUIField(
                pos,
                SharedAIKeys.P_PITCH,
                UIContents.AI_P_PITCH.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_yaw = new DoubleUIField(
                pos,
                SharedAIKeys.P_YAW,
                UIContents.AI_P_YAW.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_ag_roll = new DoubleUIField(
                pos,
                SharedAIKeys.P_AG_ROLL,
                UIContents.AI_P_AG_ROLL.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_lv_roll = new DoubleUIField(
                pos,
                SharedAIKeys.P_LV_ROLL,
                UIContents.AI_P_LV_ROLL.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_drive = new DoubleUIField(
                pos,
                SharedAIKeys.P_DRIVE,
                UIContents.AI_P_DRIVE.convertTo(Converter::titleStyle)
        );

        DoubleUIField i_drive = new DoubleUIField(
                pos,
                SharedAIKeys.I_DRIVE,
                UIContents.AI_I_DRIVE.convertTo(Converter::titleStyle)
        );

        DoubleUIField turn = new DoubleUIField(
                pos,
                SharedAIKeys.TURN_RESIST,
                UIContents.AI_TURN_RESIST.convertTo(Converter::titleStyle)
        );

        //----other----//

        Runnable alignLabels = () -> {
            Converter.alignLabel(tar, vel, tol, e_rad, twi, yaw);
            Converter.alignLabel(p_common, p_yaw, p_lv_roll, p_ag_roll, p_pitch, p_drive, i_drive, turn);
            Converter.alignLabel(flight, weapon);
        };



        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(tar, vel, tol, e_rad, twi, yaw)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        GenericUIFactory.CONTROLLER_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(p_common, p_yaw, p_lv_roll, p_ag_roll, p_pitch, p_drive, i_drive, turn)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createJetScreen(BlockPos pos){

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

        DoubleUIField e_rad = new DoubleUIField(
                pos,
                SharedAIKeys.E_RAD,
                UIContents.AI_E_RAD.convertTo(Converter::titleStyle)
        );

        // -------controller setting--------- //

        DoubleUIField p_common = new DoubleUIField(
                pos,
                SharedAIKeys.P_COMMON,
                UIContents.AI_P_COMMON.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_pitch = new DoubleUIField(
                pos,
                SharedAIKeys.P_PITCH,
                UIContents.AI_P_PITCH.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_yaw = new DoubleUIField(
                pos,
                SharedAIKeys.P_YAW,
                UIContents.AI_P_YAW.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_ag_roll = new DoubleUIField(
                pos,
                SharedAIKeys.P_AG_ROLL,
                UIContents.AI_P_AG_ROLL.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_lv_roll = new DoubleUIField(
                pos,
                SharedAIKeys.P_LV_ROLL,
                UIContents.AI_P_LV_ROLL.convertTo(Converter::titleStyle)
        );

        DoubleUIField p_drive = new DoubleUIField(
                pos,
                SharedAIKeys.P_DRIVE,
                UIContents.AI_P_DRIVE.convertTo(Converter::titleStyle)
        );

        DoubleUIField i_drive = new DoubleUIField(
                pos,
                SharedAIKeys.I_DRIVE,
                UIContents.AI_I_DRIVE.convertTo(Converter::titleStyle)
        );

        DoubleUIField turn = new DoubleUIField(
                pos,
                SharedAIKeys.TURN_RESIST,
                UIContents.AI_TURN_RESIST.convertTo(Converter::titleStyle)
        );

        //----other----//



        //----cruise-----//

        DoubleUIField sd = new DoubleUIField(
                pos,
                SharedAIKeys.STRIKE_D,
                UIContents.AI_STRIKE_D.convertTo(Converter::titleStyle)
        );

        DoubleUIField sh = new DoubleUIField(
                pos,
                SharedAIKeys.STRIKE_H,
                UIContents.AI_STRIKE_H.convertTo(Converter::titleStyle)
        );

        DoubleUIField pMax = new DoubleUIField(
                pos,
                SharedAIKeys.ENTER_MAX,
                UIContents.AI_ENTER_MAX.convertTo(Converter::titleStyle)
        );

        DoubleUIField pMin = new DoubleUIField(
                pos,
                SharedAIKeys.ENTER_MIN,
                UIContents.AI_ENTER_MIN.convertTo(Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(tar, vel, tol, rad, e_rad, twi, yaw);
            Converter.alignLabel(sd, sh, pMax, pMin);
            Converter.alignLabel(p_common, p_yaw, p_lv_roll, p_ag_roll, p_pitch, p_drive, i_drive, turn);
        };

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(tar, vel, tol, rad, e_rad, twi, yaw)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        GenericUIFactory.CONTROLLER_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(p_common, p_yaw, p_lv_roll, p_ag_roll, p_pitch, p_drive, i_drive, turn)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        GenericUIFactory.CONTROLLER_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(sd, sh, pMax, pMin)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }


    public static GenericSettingScreen createMonitorScreen(BlockPos pos){

        DoubleUIField ratio = new DoubleUIField(
                pos,
                SharedAIKeys.RATIO,
                UIContents.AI_DEATH_RATIO.convertTo(Converter::titleStyle)
        );

        BooleanUIField dead = new BooleanUIField(
                pos,
                SharedAIKeys.DEAD,
                UIContents.AI_DEAD.convertTo(Converter::titleStyle)
        );

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(ratio, dead)
                                .build()
                )
                .build();
    }


    public static GenericSettingScreen createSchematicScreen(BlockPos pos){
        StringUIField name = new StringUIField(
                pos,
                SchematicBlockEntity.NAME,
                UIContents.AI_SCHEME_NAME.convertTo(Converter::titleStyle)
        );

        StringUIField namespace = new StringUIField(
                pos,
                SchematicBlockEntity.NAMESPACE,
                UIContents.AI_SCHEME_NAMESPACE.convertTo(Converter::titleStyle)
        );

        UnitUIPanel panel = new UnitUIPanel(
                pos,
                SchematicBlockEntity.EXPORT_SCHEMATIC,
                UIContents.AI_EXPORT_SCHEME.convertTo(Converter::titleStyle)
        );

        UnitUIPanel repair = new UnitUIPanel(
                pos,
                SchematicBlockEntity.DEPLOY,
                UIContents.AI_DEPLOY.convertTo(Converter::titleStyle)
        );

        Runnable alignLabels = () -> Converter.alignLabel(name, namespace, panel, repair);

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(namespace, name)
                                .withPort(panel, repair)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }


    public static GenericSettingScreen createAutoCannonScreen(BlockPos pos){
        DoubleUIField tol = new DoubleUIField(
                pos,
                SharedAIKeys.TOL,
                UIContents.AI_TOL.convertTo(Converter::titleStyle)
        );

        DoubleUIField fire = new DoubleUIField(
                pos,
                SharedAIKeys.FIRE_RATE,
                UIContents.AI_FIRE_RATE.convertTo(Converter::titleStyle)
        );

        DoubleUIField vel = new DoubleUIField(
                pos,
                SharedAIKeys.PROJ_VEL,
                UIContents.AI_PROJ_VEL.convertTo(Converter::titleStyle)
        );

        DoubleUIField spr = new DoubleUIField(
                pos,
                SharedAIKeys.SPREAD,
                UIContents.AI_SPREAD.convertTo(Converter::titleStyle)
        );

        DoubleUIField dis = new DoubleUIField(
                pos,
                SharedAIKeys.DISTANCE,
                UIContents.AI_DISTANCE.convertTo(Converter::titleStyle)
        );

        BooleanUIField arrow = new BooleanUIField(
                pos,
                SharedAIKeys.ARROW,
                UIContents.AI_DB_ARROW.convertTo(Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(tol, fire, vel, spr, dis, arrow);
        };

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(tol, fire, vel, spr, dis, arrow)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createDirectDeployerScreen(BlockPos pos){
        StringUIField name = new StringUIField(
                pos,
                SchematicBlockEntity.NAME,
                UIContents.AI_SCHEME_NAME.convertTo(Converter::titleStyle)
        );

        StringUIField namespace = new StringUIField(
                pos,
                SchematicBlockEntity.NAMESPACE,
                UIContents.AI_SCHEME_NAMESPACE.convertTo(Converter::titleStyle)
        );

        Vector3dUIField offset_pos = new Vector3dUIField(
                pos,
                SharedAIKeys.POS,
                Converter.convert(UIContents.AI_DEPLOY_POS, Converter::titleStyle),
                25
        );

        Vector3dUIField offset_rot = new Vector3dUIField(
                pos,
                SharedAIKeys.ROT,
                Converter.convert(UIContents.AI_DEPLOY_ROT, Converter::titleStyle),
                25
        );

        BooleanUIField tran = new BooleanUIField(
                pos,
                SharedAIKeys.LOC,
                UIContents.AI_LOCAL.convertTo(Converter::titleStyle)
        );

        BooleanUIField inherit = new BooleanUIField(
                pos,
                SharedAIKeys.IHR,
                UIContents.AI_INHERIT.convertTo(Converter::titleStyle)
        );

        UnitUIPanel panel = new UnitUIPanel(
                pos,
                SharedAIKeys.ACTION,
                UIContents.AI_DEPLOY.convertTo(Converter::titleStyle)
        );


        Runnable alignLabels = () -> {
            Converter.alignLabel(name, namespace);
            Converter.alignLabel(offset_pos, offset_rot, tran, inherit, panel);
        };

        return new GenericSettingScreen.builder(pos)
                .withTab(
                        GenericUIFactory.GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(namespace, name, offset_pos, offset_rot, tran, inherit)
                                .withPort(panel)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .build();

    }

}

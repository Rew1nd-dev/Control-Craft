package com.verr1.controlcraft.content.gui.factory;

import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.blocks.anchor.AnchorBlockEntity;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.content.blocks.jet.JetBlockEntity;
import com.verr1.controlcraft.content.blocks.kinetic.resistor.KineticResistorBlockEntity;
import com.verr1.controlcraft.content.blocks.propeller.PropellerBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.legacy.PeripheralInterfaceBlockEntity_;
import com.verr1.controlcraft.content.blocks.spatial.SpatialAnchorBlockEntity;
import com.verr1.controlcraft.content.gui.layouts.element.*;
import com.verr1.controlcraft.content.gui.layouts.VerticalFlow;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.content.gui.layouts.preset.TerminalDeviceUIField;
import com.verr1.controlcraft.content.gui.screens.GenericSettingScreen;
import com.verr1.controlcraft.content.gui.layouts.preset.DynamicControllerUIField;
import com.verr1.controlcraft.content.gui.layouts.preset.SpatialScheduleUIField;
import com.verr1.controlcraft.content.legacy.TerminalDeviceUIField__;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.api.delegate.ITerminalDevice;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.*;
import com.verr1.controlcraft.registry.ControlCraftBlocks;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;

import java.util.*;

import static com.verr1.controlcraft.content.blocks.flap.FlapBearingBlockEntity.*;
import static com.verr1.controlcraft.content.gui.layouts.api.ISerializableSchedule.SCHEDULE;


/*
 *   If you want a dynamic view
 *   1.  make a syncTask to let server sync your data to client, or make this field auto synced on server side
 *   2.  override the onScreenTick() to call readToLayout()
 * */

public class GenericUIFactory {
    public static Component NOT_FOUND = Component.literal("Not Found").withStyle(s -> s.withColor(ChatFormatting.RED));


    public static Descriptive<TabType> GENERIC_SETTING_TAB = Converter.convert(TabType.GENERIC, s -> s, s -> s, s -> s.withColor(ChatFormatting.GOLD).withBold(true).withItalic(true));

    public static Descriptive<TabType> REDSTONE_TAB = Converter.convert(TabType.REDSTONE, s -> s, s -> s, s -> s.withColor(ChatFormatting.GOLD).withBold(true).withItalic(true));

    public static Descriptive<TabType> CONTROLLER_TAB = Converter.convert(TabType.CONTROLLER, s -> s, s -> s, s -> s.withColor(ChatFormatting.GOLD).withBold(true).withItalic(true));

    public static Descriptive<TabType> REMOTE_TAB = Converter.convert(TabType.REMOTE, s -> s, s -> s, s -> s.withColor(ChatFormatting.GOLD).withBold(true).withItalic(true));


    public static GenericSettingScreen createAnchorScreen(BlockPos boundAnchorPos){

        DoubleUIField air_resist = new DoubleUIField(
                boundAnchorPos,
                AnchorBlockEntity.AIR_RESISTANCE,
                Converter.convert(SlotType.AIR_RESISTANCE, Converter::titleStyle)
        );

        DoubleUIField extra_gravity = new DoubleUIField(
                boundAnchorPos,
                AnchorBlockEntity.EXTRA_GRAVITY,
                Converter.convert(SlotType.EXTRA_GRAVITY, Converter::titleStyle)
        );

        DoubleUIField rot_damp = new DoubleUIField(
                boundAnchorPos,
                AnchorBlockEntity.ROTATIONAL_RESISTANCE,
                Converter.convert(SlotType.ROTATIONAL_RESISTANCE, Converter::titleStyle)
        );

        BooleanUIField resist_at_pos = new BooleanUIField(
                boundAnchorPos,
                AnchorBlockEntity.RESISTANCE_AT_POS,
                Converter.convert(UIContents.ANCHOR_RESISTANCE_AT_POS, Converter::titleStyle)
        );

        BooleanUIField gravity_at_pos = new BooleanUIField(
                boundAnchorPos,
                AnchorBlockEntity.GRAVITY_AT_POS,
                Converter.convert(UIContents.ANCHOR_EXTRA_GRAVITY_AT_POS, Converter::titleStyle)
        );

        Converter.alignLabel(air_resist, extra_gravity, rot_damp);
        Converter.alignLabel(resist_at_pos, gravity_at_pos);

        return new GenericSettingScreen.builder(boundAnchorPos)
                .withRenderedStack(ControlCraftBlocks.ANCHOR_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundAnchorPos)
                                .withPort(AnchorBlockEntity.AIR_RESISTANCE, air_resist)
                                .withPort(AnchorBlockEntity.EXTRA_GRAVITY, extra_gravity)
                                .withPort(AnchorBlockEntity.ROTATIONAL_RESISTANCE, rot_damp)
                                .withPort(AnchorBlockEntity.RESISTANCE_AT_POS, resist_at_pos)
                                .withPort(AnchorBlockEntity.GRAVITY_AT_POS, gravity_at_pos)
                                .build()
                )
                .build();
    }

    public static GenericSettingScreen createCameraScreen(BlockPos boundAnchorPos){
        BooleanUIField is_sensor = new BooleanUIField(
                boundAnchorPos,
                CameraBlockEntity.IS_ACTIVE_SENSOR,
                Converter.convert(SlotType.IS_SENSOR, Converter::titleStyle)
        );

        OptionUIField<CameraClipType> cast_ray = new OptionUIField<>(
                boundAnchorPos,
                CameraBlockEntity.RAY_TYPE,
                CameraClipType.class,
                CameraClipType.RAY,
                Converter.convert(SlotType.CAST_RAY, Converter::titleStyle)
        );

        OptionUIField<CameraClipType> ship_ray = new OptionUIField<>(
                boundAnchorPos,
                CameraBlockEntity.SHIP_TYPE,
                CameraClipType.class,
                CameraClipType.SHIP,
                Converter.convert(SlotType.CLIP_SHIP, Converter::titleStyle)
        );

        OptionUIField<CameraClipType> entity_ray = new OptionUIField<>(
                boundAnchorPos,
                CameraBlockEntity.ENTITY_TYPE,
                CameraClipType.class,
                CameraClipType.ENTITY,
                Converter.convert(SlotType.CLIP_ENTITY, Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(is_sensor, cast_ray, ship_ray, entity_ray);
            Converter.alignLabel(cast_ray.valueLabel(), ship_ray.valueLabel(), entity_ray.valueLabel());
        };

        return new GenericSettingScreen.builder(boundAnchorPos)
                .withRenderedStack(ControlCraftBlocks.CAMERA_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundAnchorPos)
                                .withPort(CameraBlockEntity.IS_ACTIVE_SENSOR, is_sensor)
                                .withPort(CameraBlockEntity.RAY_TYPE, cast_ray)
                                .withPort(CameraBlockEntity.SHIP_TYPE, ship_ray)
                                .withPort(CameraBlockEntity.ENTITY_TYPE, entity_ray)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundAnchorPos)
                )
                .build();
    }

    public static GenericSettingScreen createFlapBearingScreen(BlockPos boundPos){
        DoubleUIView angle_view = new DoubleUIView(
                boundPos,
                ANGLE,
                Converter.convert(SlotType.DEGREE, Converter::viewStyle)
        );


        DoubleUIField angle = new DoubleUIField(
                boundPos,
                ANGLE,
                Converter.convert(SlotType.DEGREE, Converter::titleStyle)
        );

        UnitUIPanel assemble = new UnitUIPanel(
                boundPos,
                SharedKeys.ASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.ASSEMBLY, Converter::titleStyle)
        );

        UnitUIPanel disassemble = new UnitUIPanel(
                boundPos,
                SharedKeys.DISASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.DISASSEMBLY, Converter::titleStyle)
        );

        Runnable alignLabels = () -> Converter.alignLabel(assemble, disassemble);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.WING_CONTROLLER_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.PLACE_HOLDER, angle_view)
                                .withPort(ANGLE, angle)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTab(
                        REMOTE_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.ASSEMBLE, assemble)
                                .withPort(SharedKeys.DISASSEMBLE, disassemble)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTickTask(createSyncTasks(boundPos, ANGLE))
                .build();
    }

    public static GenericSettingScreen createCompactFlapScreen(BlockPos boundPos){
        DoubleUIView angle_view = new DoubleUIView(
                boundPos,
                CompactFlapBlockEntity.ANGLE,
                Converter.convert(SlotType.DEGREE, Converter::viewStyle)
        );


        DoubleUIField angle = new DoubleUIField(
                boundPos,
                CompactFlapBlockEntity.ANGLE,
                Converter.convert(SlotType.DEGREE, Converter::titleStyle)
        );


        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.WING_CONTROLLER_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.PLACE_HOLDER, angle_view)
                                .withPort(CompactFlapBlockEntity.ANGLE, angle)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTickTask(createSyncTasks(boundPos, CompactFlapBlockEntity.ANGLE))
                .build();
    }

    public static GenericSettingScreen createPropellerScreen(BlockPos boundPos){
        DoubleUIView speed = new DoubleUIView(
                boundPos,
                PropellerBlockEntity.SPEED,
                Converter.convert(SlotType.SPEED, Converter::viewStyle)
        );


        DoubleUIField torque = new DoubleUIField(
                boundPos,
                PropellerBlockEntity.TORQUE,
                Converter.convert(SlotType.TORQUE, Converter::titleStyle)
        );


        DoubleUIField thrust = new DoubleUIField(
                boundPos,
                PropellerBlockEntity.THRUST,
                Converter.convert(SlotType.THRUST, Converter::titleStyle)
        );

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.PROPELLER_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(PropellerBlockEntity.SPEED, speed)
                                .withPort(PropellerBlockEntity.TORQUE, torque)
                                .withPort(PropellerBlockEntity.THRUST, thrust)
                                .build()
                )
                .withTickTask(createSyncTasks(boundPos, PropellerBlockEntity.SPEED))
                .build();

    }


    public static GenericSettingScreen createPropellerControllerScreen(BlockPos boundPos){

        var speed_view = new DoubleUIView(boundPos, SharedKeys.VALUE, Converter.convert(SlotType.SPEED, Converter::viewStyle));

        var speed = new DoubleUIField(boundPos, SharedKeys.VALUE, Converter.convert(SlotType.SPEED, Converter::titleStyle));


        Runnable alignLabels = () -> Converter.alignLabel(speed, speed_view);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.PROPELLER_CONTROLLER.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.PLACE_HOLDER, speed_view)
                                .withPort(SharedKeys.VALUE, speed)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTickTask(createSyncTasks(boundPos, SharedKeys.VALUE))
                .build();

    }

    public static GenericSettingScreen createJetScreen(BlockPos boundPos){

        var thrust_view = new DoubleUIView(boundPos, JetBlockEntity.THRUST, Converter.convert(SlotType.THRUST, Converter::viewStyle));

        var horizontal_view = new DoubleUIView(boundPos, JetBlockEntity.HORIZONTAL_ANGLE, Converter.convert(SlotType.HORIZONTAL_TILT, Converter::viewStyle));

        var vertical_view = new DoubleUIView(boundPos, JetBlockEntity.VERTICAL_ANGLE, Converter.convert(SlotType.VERTICAL_TILT, Converter::viewStyle));

        var thrust = new DoubleUIField(boundPos, JetBlockEntity.THRUST, Converter.convert(SlotType.THRUST, Converter::titleStyle));

        var horizontal = new DoubleUIField(boundPos, JetBlockEntity.HORIZONTAL_ANGLE, Converter.convert(SlotType.HORIZONTAL_TILT, Converter::titleStyle));

        var vertical = new DoubleUIField(boundPos, JetBlockEntity.VERTICAL_ANGLE, Converter.convert(SlotType.VERTICAL_TILT, Converter::titleStyle));

        Runnable alignLabels = () -> Converter.alignLabel(thrust, horizontal, vertical, thrust_view, horizontal_view, vertical_view);

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.JET_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.PLACE_HOLDER, thrust_view)
                                .withPort(SharedKeys.PLACE_HOLDER_1, horizontal_view)
                                .withPort(SharedKeys.PLACE_HOLDER_2, vertical_view)
                                .withPort(JetBlockEntity.THRUST, thrust)
                                .withPort(JetBlockEntity.HORIZONTAL_ANGLE, horizontal)
                                .withPort(JetBlockEntity.VERTICAL_ANGLE, vertical)
                                .withPreDoLayout(alignLabels)
                                .build()
                ).withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                ).withTickTask(createSyncTasks(boundPos,
                        JetBlockEntity.THRUST,
                        JetBlockEntity.HORIZONTAL_ANGLE,
                        JetBlockEntity.VERTICAL_ANGLE)
                )
                .build();

    }


    public static GenericSettingScreen createDynamicMotorScreen(BlockPos boundPos, ItemStack stack){

        var current_view = new DoubleUIView(
                boundPos,
                SharedKeys.VALUE,
                Converter.convert(UIContents.CURRENT, Converter::viewStyle)
        );  // , Math::toDegrees d -> MathUtils.clampDigit(d, 2)

        var lock_view = new BasicUIView<>(
                boundPos,
                SharedKeys.IS_LOCKED,
                Boolean.class,
                false,
                Converter.convert(UIContents.LOCKED, Converter::viewStyle),
                Converter::lockViewComponent,
                $ -> false
        );

        var target = new DoubleUIField(boundPos, SharedKeys.TARGET, Converter.convert(UIContents.TARGET, Converter::titleStyle), d -> MathUtils.clampDigit(d, 2), d -> d); //, Converter.combine(Math::toDegrees, d -> MathUtils.clampDigit(d, 2)), Math::toRadians

        var toggle_mode = new OptionUIField<>(boundPos, SharedKeys.TARGET_MODE, TargetMode.class, Converter.convert(UIContents.MODE, Converter::titleStyle));

        var toggle_cheat = new OptionUIField<>(boundPos, SharedKeys.CHEAT_MODE, CheatMode.class, Converter.convert(UIContents.CHEAT, Converter::titleStyle));

        var toggle_lock_mode = new OptionUIField<>(boundPos, SharedKeys.LOCK_MODE, LockMode.class, Converter.convert(UIContents.AUTO_LOCK, Converter::titleStyle));

        var offset_self = new Vector3dUIField(boundPos, SharedKeys.SELF_OFFSET, Converter.convert(UIContents.SELF_OFFSET, Converter::titleStyle), 25);
        var offset_comp = new Vector3dUIField(boundPos, SharedKeys.COMP_OFFSET, Converter.convert(UIContents.COMP_OFFSET, Converter::titleStyle), 25);


        var asm = new UnitUIPanel(
                boundPos,
                SharedKeys.ASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.ASSEMBLY, Converter::titleStyle)
        );

        var lock = new UnitUIPanel(
                boundPos,
                SharedKeys.LOCK,
                Double.class,
                0.0,
                Converter.convert(UIContents.LOCK, Converter::titleStyle)
        );

        var unlock = new UnitUIPanel(
                boundPos,
                SharedKeys.UNLOCK,
                Double.class,
                0.0,
                Converter.convert(UIContents.UNLOCK, Converter::titleStyle)
        );

        var disasm = new UnitUIPanel(
                boundPos,
                SharedKeys.DISASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.DISASSEMBLY, Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(current_view, lock_view, target);
            Converter.alignLabel(toggle_mode, toggle_cheat, toggle_lock_mode);
            Converter.alignLabel(toggle_mode.valueLabel(), toggle_cheat.valueLabel(), toggle_lock_mode.valueLabel());
            Converter.alignLabel(lock, unlock, asm, disasm);
        };
        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(stack)
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.VALUE, current_view)
                                .withPort(SharedKeys.IS_LOCKED, lock_view)
                                .withPort(SharedKeys.TARGET, target)
                                .withPort(SharedKeys.SELF_OFFSET, offset_self)
                                .withPort(SharedKeys.COMP_OFFSET, offset_comp)
                                .withPort(SharedKeys.TARGET_MODE, toggle_mode)
                                .withPort(SharedKeys.CHEAT_MODE, toggle_cheat)
                                .withPort(SharedKeys.LOCK_MODE, toggle_lock_mode)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTab(
                        CONTROLLER_TAB,
                        createControllerTab(boundPos)
                )
                .withTab(
                        REMOTE_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.ASSEMBLE, asm)
                                .withPort(SharedKeys.LOCK, lock)
                                .withPort(SharedKeys.UNLOCK, unlock)
                                .withPort(SharedKeys.DISASSEMBLE, disasm)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTickTask(createSyncTasks(boundPos, SharedKeys.IS_LOCKED, SharedKeys.VALUE))
                .build();

    }

    public static GenericSettingScreen createDynamicSliderScreen(BlockPos boundPos, ItemStack stack){
        var current_view = new DoubleUIView(boundPos, SharedKeys.VALUE, Converter.convert(UIContents.CURRENT, Converter::viewStyle));

        var lock_view = new BasicUIView<>(
                boundPos,
                SharedKeys.IS_LOCKED,
                Boolean.class,
                false,
                Converter.convert(UIContents.LOCKED, Converter::viewStyle),
                Converter::lockViewComponent,
                $ -> false
        );

        var target = new DoubleUIField(boundPos, SharedKeys.TARGET, Converter.convert(UIContents.TARGET, Converter::titleStyle), d -> MathUtils.clampDigit(d, 2), d -> d);

        var toggle_mode = new OptionUIField<>(boundPos, SharedKeys.TARGET_MODE, TargetMode.class, Converter.convert(UIContents.MODE, Converter::titleStyle));

        var toggle_cheat = new OptionUIField<>(boundPos, SharedKeys.CHEAT_MODE, CheatMode.class, new CheatMode[]{CheatMode.NONE, CheatMode.NO_REPULSE} , Converter.convert(UIContents.CHEAT, Converter::titleStyle));

        var toggle_lock_mode = new OptionUIField<>(boundPos, SharedKeys.LOCK_MODE, LockMode.class, Converter.convert(UIContents.AUTO_LOCK, Converter::titleStyle));

        var asm = new UnitUIPanel(
                boundPos,
                SharedKeys.ASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.ASSEMBLY, Converter::titleStyle)
        );

        var lock = new UnitUIPanel(
                boundPos,
                SharedKeys.LOCK,
                Double.class,
                0.0,
                Converter.convert(UIContents.LOCK, Converter::titleStyle)
        );

        var unlock = new UnitUIPanel(
                boundPos,
                SharedKeys.UNLOCK,
                Double.class,
                0.0,
                Converter.convert(UIContents.UNLOCK, Converter::titleStyle)
        );

        var disasm = new UnitUIPanel(
                boundPos,
                SharedKeys.DISASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.DISASSEMBLY, Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(current_view, lock_view, target);
            Converter.alignLabel(toggle_mode, toggle_cheat, toggle_lock_mode);
            Converter.alignLabel(toggle_mode.valueLabel(), toggle_cheat.valueLabel(), toggle_lock_mode.valueLabel());
            Converter.alignLabel(lock, unlock, asm, disasm);
        };
        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(stack)
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.VALUE, current_view)
                                .withPort(SharedKeys.IS_LOCKED, lock_view)
                                .withPort(SharedKeys.TARGET, target)
                                .withPort(SharedKeys.TARGET_MODE, toggle_mode)
                                .withPort(SharedKeys.CHEAT_MODE, toggle_cheat)
                                .withPort(SharedKeys.LOCK_MODE, toggle_lock_mode)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTab(
                        CONTROLLER_TAB,
                        createControllerTab(boundPos)
                )
                .withTab(
                        REMOTE_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.ASSEMBLE, asm)
                                .withPort(SharedKeys.LOCK, lock)
                                .withPort(SharedKeys.UNLOCK, unlock)
                                .withPort(SharedKeys.DISASSEMBLE, disasm)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTickTask(createSyncTasks(boundPos, SharedKeys.IS_LOCKED, SharedKeys.VALUE))
                .build();

    }




    public static GenericSettingScreen createPeripheralInterfaceScreen(BlockPos boundPos){
        var type_view = new BasicUIView<>(
                boundPos,
                PeripheralInterfaceBlockEntity.PERIPHERAL_TYPE,
                String.class,
                "Not Attached",
                Converter.convert(UIContents.TYPE, Converter::viewStyle),
                s -> Component.literal(s).withStyle(Converter::optionStyle),
                $ -> ""
        );

        var key_view = new PeripheralKeyUIView(boundPos);
        var key_field = new PeripheralKeyUIField(boundPos);

        var forced = new BooleanUIField(
                boundPos,
                PeripheralInterfaceBlockEntity.FORCED,
                Converter.convert(UIContents.FORCED, Converter::titleStyle)
        );

        var online = new UnitUIPanel(
                boundPos,
                PeripheralInterfaceBlockEntity.ONLINE,
                Double.class,
                0.0,
                Converter.convert(UIContents.ONLINE, Converter::titleStyle)
        );

        var offline = new UnitUIPanel(
                boundPos,
                PeripheralInterfaceBlockEntity.OFFLINE,
                Double.class,
                0.0,
                Converter.convert(UIContents.OFFLINE, Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(online, offline);
        };

        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.RECEIVER_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(PeripheralInterfaceBlockEntity.PERIPHERAL_TYPE, type_view)
                                .withPort(PeripheralInterfaceBlockEntity.VALID_PERIPHERAL, key_view)
                                .withPort(PeripheralInterfaceBlockEntity.PERIPHERAL, key_field)
                                .withPort(PeripheralInterfaceBlockEntity.FORCED, forced)
                                .build()
                )
                .withTab(
                        REMOTE_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(PeripheralInterfaceBlockEntity.ONLINE, online)
                                .withPort(SharedKeys.DISASSEMBLE, offline)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTickTask(createSyncTasks(boundPos, PeripheralInterfaceBlockEntity.PERIPHERAL_TYPE))
                .withTickTask(createSyncTasks(boundPos, PeripheralInterfaceBlockEntity.VALID_PERIPHERAL))
                .build();
    }


    public static GenericSettingScreen createSpatialAnchorScreen(BlockPos pos){

        var offset_field = new DoubleUIField(pos, SpatialAnchorBlockEntity.OFFSET, UIContents.SPATIAL_OFFSET.convertTo(Converter::titleStyle));

        var protocol_field = new LongUIField(pos, SpatialAnchorBlockEntity.PROTOCOL, UIContents.PROTOCOL.convertTo(Converter::titleStyle));

        var is_running_field = new BooleanUIField(pos, SpatialAnchorBlockEntity.IS_RUNNING, SlotType.IS_RUNNING.convertTo(Converter::titleStyle));

        var is_static_field = new BooleanUIField(pos, SpatialAnchorBlockEntity.IS_STATIC, SlotType.IS_STATIC.convertTo(Converter::titleStyle));




        Runnable alignLabels = () -> {
            Converter.alignLabel(offset_field, protocol_field);
            Converter.alignLabel(is_running_field, is_static_field);
        };
        return new GenericSettingScreen.builder(pos)
                .withRenderedStack(ControlCraftBlocks.SPATIAL_ANCHOR_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(pos)
                                .withPort(SpatialAnchorBlockEntity.OFFSET, offset_field)
                                .withPort(SpatialAnchorBlockEntity.PROTOCOL, protocol_field)
                                .withPort(SpatialAnchorBlockEntity.IS_RUNNING, is_running_field)
                                .withPort(SpatialAnchorBlockEntity.IS_STATIC, is_static_field)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        CONTROLLER_TAB,
                        createScheduleTab(pos)
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(pos)
                )
                .build();
    }

    public static GenericSettingScreen createKinematicDeviceScreen(BlockPos boundPos, ItemStack stack){

        var current_view = new DoubleUIView(boundPos, SharedKeys.VALUE, Converter.convert(UIContents.CURRENT, Converter::viewStyle));

        var target_field = new DoubleUIField(boundPos, SharedKeys.TARGET, Converter.convert(UIContents.TARGET, Converter::titleStyle));

        var self_offset = new Vector3dUIField(boundPos, SharedKeys.SELF_OFFSET, Converter.convert(UIContents.SELF_OFFSET, Converter::titleStyle), 25);

        var comp_offset = new Vector3dUIField(boundPos, SharedKeys.COMP_OFFSET, Converter.convert(UIContents.COMP_OFFSET, Converter::titleStyle), 25);


        var compliance_field = new DoubleUIField(
                boundPos,
                SharedKeys.COMPLIANCE,
                Converter.convert(UIContents.COMPLIANCE, Converter::titleStyle)
        );

        var toggle_mode = new OptionUIField<>(boundPos, SharedKeys.TARGET_MODE, TargetMode.class, Converter.convert(UIContents.MODE, Converter::titleStyle));

        var asm = new UnitUIPanel(
                boundPos,
                SharedKeys.ASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.ASSEMBLY, Converter::titleStyle)
        );

        var disasm = new UnitUIPanel(
                boundPos,
                SharedKeys.DISASSEMBLE,
                Double.class,
                0.0,
                Converter.convert(UIContents.DISASSEMBLY, Converter::titleStyle)
        );

        Runnable alignLabels = () -> {
            Converter.alignLabel(current_view, target_field, compliance_field, toggle_mode);
            Converter.alignLabel(asm, disasm);
        };

        return new GenericSettingScreen.builder(boundPos)
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.VALUE, current_view)
                                .withPort(SharedKeys.TARGET, target_field)
                                .withPort(SharedKeys.COMPLIANCE, compliance_field)
                                .withPort(SharedKeys.SELF_OFFSET, self_offset)
                                .withPort(SharedKeys.COMP_OFFSET, comp_offset)
                                .withPort(SharedKeys.TARGET_MODE, toggle_mode)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTab(
                        REMOTE_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(SharedKeys.ASSEMBLE, asm)
                                .withPort(SharedKeys.DISASSEMBLE, disasm)
                                .withPreDoLayout(alignLabels)
                                .build()
                )
                .withRenderedStack(stack)
                .withTickTask(createSyncTasks(boundPos, SharedKeys.VALUE))
                .build();
    }

    public static GenericSettingScreen createKineticResistorScreen(BlockPos boundPos){
        var ratio = new DoubleUIField(boundPos, KineticResistorBlockEntity.RATIO, Converter.convert(SlotType.RATIO, Converter::titleStyle));



        return new GenericSettingScreen.builder(boundPos)
                .withRenderedStack(ControlCraftBlocks.KINETIC_RESISTOR_BLOCK.asStack())
                .withTab(
                        GENERIC_SETTING_TAB,
                        new VerticalFlow.builder(boundPos)
                                .withPort(KineticResistorBlockEntity.RATIO, ratio)
                                .build()
                )
                .withTab(
                        REDSTONE_TAB,
                        createTerminalDeviceTab(boundPos)
                )
                .withTickTask(createSyncTasks(boundPos, KineticResistorBlockEntity.RATIO))
                .build();
    }

    public static Runnable createSyncTasks(BlockPos boundPos, NetworkKey... keys){
        return () -> boundBlockEntity(boundPos, INetworkHandle.class).ifPresent(
                be -> be.handler().request(keys)
        );
    }


    public static VerticalFlow createTerminalDeviceTab__(BlockPos boundPos){
        return new VerticalFlow.builder(boundPos)
                .withPort(
                        ITerminalDevice.FIELD__,
                        new TerminalDeviceUIField__(boundPos)
                ).build();
    }

    public static VerticalFlow createTerminalDeviceTab(BlockPos boundPos){
        return new VerticalFlow.builder(boundPos)
                .withPort(
                        IReceiver.FIELD,
                        new TerminalDeviceUIField(boundPos)
                ).build();
    }

    public static VerticalFlow createControllerTab(BlockPos boundPos){
        return new VerticalFlow.builder(boundPos)
                .withPort(
                        SharedKeys.CONTROLLER,
                        new DynamicControllerUIField(boundPos, 30)
                ).build();
    }


    public static VerticalFlow createScheduleTab(BlockPos boundPos){
        return new VerticalFlow.builder(boundPos)
                .withPort(
                        SCHEDULE,
                        new SpatialScheduleUIField(boundPos, 25)
                ).build();
    }

    public static <T> Optional<T> boundBlockEntity(BlockPos p, Class<T> clazz){
        Minecraft mc = Minecraft.getInstance();
        return BlockEntityGetter.getLevelBlockEntityAt(mc.level, p, clazz);
    }


}

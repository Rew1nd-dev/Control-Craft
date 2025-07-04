package com.verr1.controlcraft.ponder.scenes;

import com.simibubi.create.AllBlocks;
import com.simibubi.create.content.kinetics.simpleRelays.CogWheelBlock;
import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerCompact;
import com.verr1.controlcraft.content.links.CimulinkBlock;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.util.CimulinkPonderUtil;
import com.verr1.controlcraft.registry.ControlCraftBlocks;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;

import static com.simibubi.create.content.kinetics.base.DirectionalKineticBlock.FACING;
import static com.verr1.controlcraft.ponder.scenes.BasicScene.*;

public class ProxyScene {

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        var proxy = of(3, 1, 3);
        var plant = proxy.south();
        var plantTop = plant.above();

        var in = proxy.west().west();
        var out = proxy.east().east();

        CimulinkPonderUtil cu = new CimulinkPonderUtil(scene, util, "proxy");

        cu
                .init()
                .setBlock(Constants.PROXY.setValue(CimulinkBlock.FACING, Direction.NORTH), proxy).idle(4)
                .setBlock(AllBlocks.LARGE_COGWHEEL.getDefaultState().setValue(CogWheelBlock.AXIS, Direction.Axis.Z), plantTop).idle(4)
                .setBlock(AllBlocks.ROTATION_SPEED_CONTROLLER.getDefaultState(), plant).idle(4)
                .setBlock(Constants.INPUT, in)
                .setBlock(Constants.OUTPUT, out)
                .inst(addWire("i", in, proxy).endDirection(Direction.NORTH)).idle(4)
                .inst(addWire("o", proxy, out).startDirection(Direction.NORTH)).idle(READING_TIME);

        cu
                .frame()
                .text("This is a Proxy", proxy, READING_TIME).idle(READING_TIME)
                .text("It is used to expose machine input and output ports", proxy, READING_TIME).idle(READING_TIME)
                .text("Let's say it's attached to a speed controller",  plant, READING_TIME / 2).idle(READING_TIME / 2)
                .text("You can now set speed and get speed from proxy ports", plant, READING_TIME / 2).idle(READING_TIME / 2)
                .text("set input to 32", in, READING_TIME / 4).idle(READING_TIME / 3);

        scene.world.setKineticSpeed(util.select.position(plant), 32);
        scene.world.setKineticSpeed(util.select.position(plantTop), 32);

        cu
                .text("now speed is set to 32", plant, READING_TIME / 3).idle(READING_TIME / 3)
                .text("you can get speed = 32 here", out, READING_TIME / 3).idle(READING_TIME / 2);

        cu
                .frame()
                .clearBlock(util.select.position(plantTop))
                .setBlock(ControlCraftBlocks.KINETIC_RESISTOR_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant)
                .text("For resistor, you can get and set RATIO in proxy port", proxy, READING_TIME).idle(READING_TIME);

        cu
                .frame()
                .setBlock(ControlCraftBlocks.PROPELLER_CONTROLLER.getDefaultState().setValue(FACING, Direction.UP), plant)
                .setBlock(ControlCraftBlocks.PROPELLER_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plantTop).idle(READING_TIME / 2)
                .text("For propeller, you can get and set SPEED in proxy port", proxy, READING_TIME).idle(READING_TIME)
                .clearBlock(util.select.position(plantTop));

        BlockState mount_0 = CreateBigCannonsCompact.cannonMountBlock(0);
        BlockState mount_1 = CreateBigCannonsCompact.cannonMountBlock(1);
        BlockState lectern = TweakControllerCompact.lecternBlock();

        if(mount_0 != null){
            cu.setBlock(mount_0, plant).idle(READING_TIME_33);
            if(mount_1 != null){
                cu.setBlock(mount_1, plant).idle(READING_TIME_33);
            }
            cu.text("For cannons, you can get PITCH and YAW in proxy port", proxy, READING_TIME).idle(READING_TIME_133)
            ;
        }
        if(lectern != null){
            cu
                    .setBlock(lectern, plant).idle(READING_TIME_33)
                    .text("For tweaked lectern controller, you can get player INPUT AXIS in proxy port", proxy, READING_TIME).idle(READING_TIME_133)
            ;
        }


        cu
                .setBlock(ControlCraftBlocks.JET_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant).idle(READING_TIME / 3)
                .setBlock(ControlCraftBlocks.SERVO_MOTOR_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant).idle(READING_TIME / 3)
                .setBlock(ControlCraftBlocks.JOINT_MOTOR_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant).idle(READING_TIME / 3)
                .setBlock(ControlCraftBlocks.CAMERA_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant).idle(READING_TIME / 3)
                .setBlock(ControlCraftBlocks.COMPACT_FLAP_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant).idle(READING_TIME / 3)
                .setBlock(ControlCraftBlocks.WING_CONTROLLER_BLOCK.getDefaultState().setValue(FACING, Direction.UP), plant).idle(READING_TIME / 3);



        cu
                .text("Just try it yourself to see what you can get/set!", proxy, READING_TIME / 2).idle(READING_TIME);

        cu.end();
    }

}

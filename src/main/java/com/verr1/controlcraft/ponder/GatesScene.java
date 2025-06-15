package com.verr1.controlcraft.ponder;

import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.verr1.controlcraft.registry.CimulinkBlocks;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.network.chat.Component;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.LeverBlock;
import net.minecraft.world.level.block.RedstoneLampBlock;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;

import static com.simibubi.create.content.kinetics.base.DirectionalKineticBlock.FACING;

public class GatesScene {
    public static final String ID = "cimulink/gates";
    public static final String AND_OR_XOR = "cimulink/and_or_xor";

    public static void scene(SceneBuilder scene, SceneBuildingUtil util){
        scene.title(ID, "Logic Gates");
        scene.configureBasePlate(0, 0, 7);
        scene.scaleSceneView(0.9F);
        scene.showBasePlate();
        // scene.world.showSection(util.select.layer(0), Direction.UP);
        scene.world.showSection(util.select.layer(1), Direction.UP);

        BlockPos leverPos = new BlockPos(1, 1, 4);
        BlockPos inputPos = new BlockPos(1, 1, 3);
        BlockPos outputPos = new BlockPos(4, 1, 3);


        scene.idle(20);
        scene.overlay.showText(50)
                .text("Input Port Receives Neighbor Redstone Signal")
                .pointAt(inputPos.getCenter())
                .attachKeyFrame()
                .placeNearTarget();

        scene.idle(10);
        // scene.world.showSection(util.select.position(1, 1, 3), Direction.UP);

        scene.world.modifyBlock(leverPos, b -> b.setValue(LeverBlock.POWERED, true), true);



        scene.idle(10);

        scene.overlay.showText(40)
                .text("Input: 15")
                .pointAt(inputPos.getCenter())
                .attachKeyFrame()
                .placeNearTarget();

        scene.idle(40);



        scene.overlay.showText(40)
                .text("Signal Transmitted Through Wire")
                .pointAt(outputPos.getCenter())
                .attachKeyFrame()
                .placeNearTarget();

        scene.idle(40);

        scene.overlay.showText(40)
                .text("Output: 15")
                .pointAt(outputPos.getCenter())
                .attachKeyFrame()
                .placeNearTarget();

        scene.world.toggleRedstonePower(util.select.position(outputPos));

    }

    public static void scene_1(SceneBuilder scene, SceneBuildingUtil util){
        scene.title(ID, "Logic Gates");
        scene.configureBasePlate(0, 0, 7);
        scene.scaleSceneView(0.9F);
        scene.showBasePlate();
        // scene.world.showSection(util.select.layer(0), Direction.UP);
        scene.world.showSection(util.select.layer(1), Direction.UP);
        scene.world.showSection(util.select.layer(2), Direction.UP);

        BlockPos gatePos = new BlockPos(2, 1, 3);
        BlockPos input0Pos = new BlockPos(1, 1, 4);
        BlockPos input1Pos = new BlockPos(1, 1, 2);
        BlockPos lever0Pos = new BlockPos(0, 1, 4);
        BlockPos lever1Pos = new BlockPos(0, 1, 2);
        BlockPos outputPos = new BlockPos(4, 1, 3);
        BlockPos lampPos = new BlockPos(5, 1, 4);


        scene.idle(20);
        scene.overlay.showText(50)
                .text("AND Gates Applies Output When Both Input > 0.5")
                .pointAt(gatePos.getCenter())
                .attachKeyFrame()
                .placeNearTarget();

        scene.idle(80);

        scene.world.modifyBlock(lever0Pos, b -> b.setValue(LeverBlock.POWERED, true), true);
        scene.overlay.showText(40).text("Input: 15").pointAt(input0Pos.getCenter()).placeNearTarget();

        scene.overlay.showText(40).text("Input: 0").pointAt(input1Pos.getCenter()).placeNearTarget();

        scene.overlay.showText(40).text("Output: 0").pointAt(outputPos.getCenter()).placeNearTarget();

        scene.idle(40);

        scene.overlay.showText(40).text("Input: 15").pointAt(input0Pos.getCenter()).placeNearTarget();

        scene.world.modifyBlock(lever1Pos, b -> b.setValue(LeverBlock.POWERED, true), true);
        scene.overlay.showText(40).text("Input: 15").pointAt(input1Pos.getCenter()).placeNearTarget();

        scene.overlay.showText(40).text("Output: 1").pointAt(outputPos.getCenter()).placeNearTarget();
        scene.world.modifyBlock(lampPos, b -> b.setValue(RedstoneLampBlock.LIT, true), true);

        scene.idle(40);

        scene.world.modifyBlock(lever0Pos, b -> b.setValue(LeverBlock.POWERED, false), true);
        scene.overlay.showText(40).text("Input: 0").pointAt(input0Pos.getCenter()).placeNearTarget();


        scene.overlay.showText(40).text("Input: 15").pointAt(input1Pos.getCenter()).placeNearTarget();

        scene.overlay.showText(40).text("Output: 0").pointAt(outputPos.getCenter()).placeNearTarget();
        scene.world.modifyBlock(lampPos, b -> b.setValue(RedstoneLampBlock.LIT, false), true);

        scene.idle(40);
    }
}

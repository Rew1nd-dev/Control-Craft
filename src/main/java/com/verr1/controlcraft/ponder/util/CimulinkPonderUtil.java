package com.verr1.controlcraft.ponder.util;

import com.simibubi.create.content.redstone.analogLever.AnalogLeverBlockEntity;
import com.simibubi.create.content.redstone.nixieTube.NixieTubeBlockEntity;
import com.simibubi.create.foundation.ponder.PonderPalette;
import com.simibubi.create.foundation.ponder.SceneBuilder;
import com.simibubi.create.foundation.ponder.SceneBuildingUtil;
import com.simibubi.create.foundation.ponder.Selection;
import com.simibubi.create.foundation.ponder.element.InputWindowElement;
import com.simibubi.create.foundation.ponder.instruction.PonderInstruction;
import com.simibubi.create.foundation.utility.Pointing;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.ponder.elements.CimulinkPonderOutliner;
import com.verr1.controlcraft.ponder.scenes.BasicScene;
import com.verr1.controlcraft.ponder.Constants;
import com.verr1.controlcraft.ponder.elements.PlainTextElement;
import com.verr1.controlcraft.ponder.instructions.CimulinkWireInstruction;
import com.verr1.controlcraft.ponder.instructions.PlainTextInstruction;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.Vec3;

import java.util.function.UnaryOperator;

import static com.verr1.controlcraft.ponder.scenes.BasicScene.lit;

public record CimulinkPonderUtil(SceneBuilder scene, SceneBuildingUtil util) {

    public CimulinkPonderUtil setBlock(BlockState block, BlockPos pos) {
        scene.addInstruction(BasicScene.set(util.select.position(pos), block));
        return this;
    }

    public CimulinkPonderUtil setBlock(UnaryOperator<BlockState> blockFunc, BlockPos pos) {
        scene.addInstruction(BasicScene.set(util.select.position(pos), blockFunc));
        return this;
    }

    public CimulinkPonderUtil clearBlock(Selection selection) {
        scene.addInstruction(BasicScene.set(selection, Blocks.AIR.defaultBlockState()));
        return this;
    }

    public CimulinkPonderUtil clearBlock(BlockPos from, BlockPos to) {
        scene.addInstruction(BasicScene.set(util.select.fromTo(from, to), Blocks.AIR.defaultBlockState()));
        return this;
    }

    public CimulinkPonderUtil setBlock(BlockState block, BlockPos... poses) {
        for (var p : poses){
            setBlock(block, p);
        }
        return this;
    }

    public CimulinkPonderUtil selectArea(BlockPos from, BlockPos to, PonderPalette color, Object slot, int duration){
        scene.overlay.showOutline(color, slot, util.select.fromTo(from, to), duration);
        return this;
    }

    public CimulinkPonderUtil selectArea(Selection selection, PonderPalette color, Object slot, int duration){
        scene.overlay.showOutline(color, slot, selection, duration);
        return this;
    }

    public CimulinkPonderUtil removeBlock(BlockPos pos) {
        return setBlock(Blocks.AIR.defaultBlockState(), pos);
    }

    public CimulinkPonderUtil click(BlockPos pos, int duration) {
        var c = new InputWindowElement(
                util.vector.blockSurface(pos, Direction.WEST),
                Pointing.LEFT
        ).withItem(Constants.AWE);
        scene.overlay.showControls(c, duration);
        return this;
    }

    public CimulinkPonderUtil click(BlockPos pos, ItemStack stack, int duration) {
        var c = new InputWindowElement(
                util.vector.blockSurface(pos, Direction.WEST),
                Pointing.LEFT
        ).withItem(stack);
        scene.overlay.showControls(c, duration);
        return this;
    }

    public CimulinkPonderUtil idle(int ticks) {
        scene.idle(ticks);
        return this;
    }

    public CimulinkPonderUtil init(){
        scene.title(BasicScene.EMPTY, "Cimulink");
        scene.configureBasePlate(0, 0, 7);
        scene.scaleSceneView(0.9F);
        scene.showBasePlate();
        // scene.world.showSection(util.select.layer(0), Direction.UP);
        scene.world.showSection(util.select.everywhere(), Direction.UP);
        CimulinkPonderOutliner.getOrCreate().clear();
        return initWireRendering();
    }

    public CimulinkPonderUtil initWireRendering(){
        scene.addInstruction(CimulinkWireInstruction.initInstruction());
        return this;
    }

    public CimulinkPonderUtil text(String text, Vec3 pointAt, int duration) {
        PlainTextElement textWindowElement = new PlainTextElement();
        textWindowElement.new Builder().text(text).pointAt(pointAt).placeNearTarget();
        scene.addInstruction(new PlainTextInstruction(textWindowElement, duration));
        return this;
    }

    public CimulinkPonderUtil text(String text, BlockPos pointAt, int duration) {
        return text(text, pointAt.getCenter(), duration);
    }

    public CimulinkPonderUtil text(Descriptive<?> text, Vec3 pointAt, int duration) {
        PlainTextElement textWindowElement = new PlainTextElement();
        textWindowElement.new Builder().text(text).pointAt(pointAt).placeNearTarget();
        scene.addInstruction(new PlainTextInstruction(textWindowElement, duration));
        return this;
    }

    public CimulinkPonderUtil text(Descriptive<?> text, BlockPos pointAt, int duration) {
        return text(text, pointAt.getCenter(), duration);
    }

    public CimulinkPonderUtil frame() {
        scene.addLazyKeyframe();
        return this;
    }

    public CimulinkPonderUtil inst(PonderInstruction inst) {
        scene.addInstruction(inst);
        return this;
    }

    public CimulinkPonderUtil power(BlockPos pos, int level){
        scene.world.modifyBlockEntityNBT(util.select.position(pos), AnalogLeverBlockEntity.class,
                nbt -> nbt.putInt("State", level));
        return this;
    }



    public CimulinkPonderUtil showPower(BlockPos pos, int level){
        scene.world.modifyBlockEntityNBT(util.select.position(pos), NixieTubeBlockEntity.class, nbt -> nbt.putInt("RedstoneStrength", level));
        return this;
    }
}

package com.verr1.controlcraft.ponder.elements;

import com.mojang.blaze3d.vertex.PoseStack;
import com.simibubi.create.foundation.ponder.PonderScene;
import com.simibubi.create.foundation.ponder.PonderWorld;
import com.simibubi.create.foundation.ponder.element.PonderSceneElement;
import com.verr1.controlcraft.foundation.data.render.RenderableOutline;
import com.verr1.controlcraft.foundation.managers.render.BezierOutliner;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.world.phys.Vec3;

import java.util.function.Supplier;

public class CimulinkPonderOutliner extends PonderSceneElement {
    private static CimulinkPonderOutliner INSTANCE;
    private final BezierOutliner outliner = new BezierOutliner();

    public static CimulinkPonderOutliner getOrCreate(){
        if (INSTANCE == null) {
            INSTANCE = new CimulinkPonderOutliner();
        }
        return INSTANCE;
    }

    public CimulinkPonderOutliner resetVisibility(){
        setVisible(true);
        return this;
    }

    @Override
    public boolean isVisible() {
        return true;
    }

    @Override
    public void tick(PonderScene scene) {
        super.tick(scene);
        outliner.tickOutlines();
    }

    public BezierOutliner.OutlineEntry showLine(Object slot, Supplier<RenderableOutline> factory) {
        return outliner.showLine(slot, factory);
    }

    public void remove(Object slot) {
        outliner.remove(slot);
    }

    public void clear(){
        outliner.clear();
    }

    public RenderableOutline retrieveLine(Object slot){
        return outliner.retrieveLine(slot);
    }

    @Override
    public void renderFirst(PonderWorld world, MultiBufferSource buffer, PoseStack ms, float pt) {
        outliner.renderOutlines(ms, buffer, new Vec3(0, 0, 0), pt);
    }

    @Override
    public void renderLayer(PonderWorld world, MultiBufferSource buffer, RenderType type, PoseStack ms, float pt) {

    }

    @Override
    public void renderLast(PonderWorld world, MultiBufferSource buffer, PoseStack ms, float pt) {

    }
}

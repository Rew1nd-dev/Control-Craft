package com.verr1.controlcraft.foundation.managers.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.simibubi.create.foundation.outliner.Outline;
import com.simibubi.create.foundation.render.SuperRenderTypeBuffer;
import com.verr1.controlcraft.foundation.data.render.BezierCurveEntry;
import com.verr1.controlcraft.foundation.data.render.Line;
import com.verr1.controlcraft.foundation.data.render.RayLerpHelper;
import com.verr1.controlcraft.foundation.data.render.RenderableOutline;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.world.phys.Vec3;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.function.Supplier;

public class BezierOutliner {

    private final Map<Object, OutlineEntry> outlines = Collections.synchronizedMap(new HashMap<>());

    public void tickOutlines() {
        Iterator<OutlineEntry> iterator = outlines.values()
                .iterator();
        while (iterator.hasNext()) {
            OutlineEntry entry = iterator.next();
            entry.tick();
            if (!entry.isAlive())
                iterator.remove();
        }
    }

    public OutlineEntry showLine(Object slot, Supplier<RenderableOutline> factory) {
        OutlineEntry entry = outlines.computeIfAbsent(slot, k -> new OutlineEntry(factory.get()));
        entry.ticksTillRemoval = 10;
        return entry;
    }

    public RenderableOutline retrieveLine(Object slot){
        if(!outlines.containsKey(slot))return null;
        return outlines.get(slot).outline;
    }

    public void changeLine(Object slot, Supplier<RenderableOutline> factory) {
        OutlineEntry entry = new OutlineEntry(factory.get());
        outlines.put(slot, entry);
        entry.ticksTillRemoval = 10;
    }

    public void remove(Object slot){
        outlines.remove(slot);
    }

    public void clear(){
        outlines.clear();
    }

    public void renderOutlines(PoseStack ms, MultiBufferSource buffer, Vec3 camera, float pt) {
        outlines.forEach((key, entry) -> {
            RenderableOutline outline = entry.getOutline();
            outline.render(ms, buffer, camera, pt);
        });
    }

    public static class OutlineEntry{
        public static final int FADE_TICKS = 8;

        private final RenderableOutline outline;
        private int ticksTillRemoval = 1;
        private boolean eternal = false;


        public OutlineEntry(RenderableOutline outline) {
            this.outline = outline;
        }

        public RenderableOutline getOutline() {
            return outline;
        }

        public int getTicksTillRemoval() {
            return ticksTillRemoval;
        }

        public OutlineEntry setLive(int ticksTillRemoval){
            this.ticksTillRemoval = ticksTillRemoval;
            return this;
        }

        public OutlineEntry eternal(boolean eternal){
            this.eternal = eternal;
            return this;
        }

        public boolean isAlive() {
            return ticksTillRemoval >= -FADE_TICKS;
        }

        public boolean isFading() {
            return ticksTillRemoval < 0;
        }

        public void tick() {
            if(!eternal)ticksTillRemoval--;
            outline.tick();
        }
    }
}

package com.verr1.controlcraft.ponder.instructions;

import com.simibubi.create.foundation.ponder.PonderScene;
import com.simibubi.create.foundation.ponder.Selection;
import com.simibubi.create.foundation.ponder.element.OutlinerElement;
import com.simibubi.create.foundation.ponder.element.TextWindowElement;
import com.simibubi.create.foundation.ponder.instruction.FadeInOutInstruction;
import com.verr1.controlcraft.ponder.elements.PlainTextElement;

public class PlainTextInstruction extends FadeInOutInstruction {

    private PlainTextElement element;
    private OutlinerElement outline;

    public PlainTextInstruction(PlainTextElement element, int duration) {
        super(duration);
        this.element = element;
    }

    public PlainTextInstruction(PlainTextElement element, int duration, Selection selection) {
        this(element, duration);
        outline = new OutlinerElement(o -> selection.makeOutline(o)
                .lineWidth(1 / 16f));
    }

    @Override
    public void tick(PonderScene scene) {
        super.tick(scene);
        if (outline != null)
            outline.setColor(element.getColor());
    }

    @Override
    protected void show(PonderScene scene) {
        scene.addElement(element);
        element.setVisible(true);
        if (outline != null) {
            scene.addElement(outline);
            outline.setFade(1);
            outline.setVisible(true);
        }
    }

    @Override
    protected void hide(PonderScene scene) {
        element.setVisible(false);
        if (outline != null) {
            outline.setFade(0);
            outline.setVisible(false);
        }
    }

    @Override
    protected void applyFade(PonderScene scene, float fade) {
        element.setFade(fade);
    }


}

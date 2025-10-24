package com.verr1.controlcraft.content.gui.widgets;

import com.simibubi.create.foundation.gui.element.ScreenElement;
import com.simibubi.create.foundation.gui.widget.SelectionScrollInput;
import com.verr1.controlcraft.content.gui.layouts.api.SizedScreenElement;
import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class IconSelectionScrollInput extends SelectionScrollInput {

    private final ScreenElement icon;
    private final ScreenElement hovered;
    private Function<Integer, List<Component>> mutableDescriptions = i -> List.of();
    private final ArrayList<Component> commonDescriptions = new ArrayList<>();

    public IconSelectionScrollInput(int xIn, int yIn, int widthIn, int heightIn, ScreenElement icon) {
        super(xIn, yIn, widthIn, heightIn);
        this.icon = icon;
        this.hovered = icon;
    }

    public IconSelectionScrollInput(int xIn, int yIn, int widthIn, int heightIn, ScreenElement icon, ScreenElement hovered) {
        super(xIn, yIn, widthIn, heightIn);
        this.icon = icon;
        this.hovered = hovered;
    }

    public IconSelectionScrollInput(SizedScreenElement icon) {
        super(0, 0, icon.width(), icon.height());
        this.icon = icon;
        this.hovered = icon;
    }

    public IconSelectionScrollInput(SizedScreenElement icon, SizedScreenElement hovered) {
        super(0, 0, icon.width(), icon.height());
        this.icon = icon;
        this.hovered = hovered;
    }

    public IconSelectionScrollInput withDescriptions(List<Component> descriptions){
        this.commonDescriptions.clear();
        this.commonDescriptions.addAll(descriptions);
        return this;
    }

    public IconSelectionScrollInput withDescription(Component description){
        this.commonDescriptions.clear();
        this.commonDescriptions.add(description);
        return this;
    }

    @Override
    protected void doRender(@NotNull GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        super.doRender(graphics, mouseX, mouseY, partialTicks);
        if(isHovered){
            hovered.render(graphics, getX(), getY());
        }else{
            icon.render(graphics, getX(), getY());
        }
    }

    public IconSelectionScrollInput withOptionDescriptions(Function<Integer, List<Component>> descriptions){
        this.mutableDescriptions = descriptions;
        return this;
    }

    @Override
    protected void updateTooltip() {
        super.updateTooltip();
        List<Component> stateDescriptions = this.mutableDescriptions.apply(getState());

        if (!commonDescriptions.isEmpty()){
            toolTip.add(Component.translatable("tooltip.description").withStyle(s -> s.withBold(true).withColor(ChatFormatting.GOLD).withItalic(true)));
            toolTip.addAll(commonDescriptions);
        }
        if(!stateDescriptions.isEmpty()){
            toolTip.add(Component.translatable("tooltip.forthisoption").withStyle(s -> s.withBold(true).withColor(ChatFormatting.GOLD).withItalic(true)));
            toolTip.addAll(stateDescriptions);
        }
    }

}

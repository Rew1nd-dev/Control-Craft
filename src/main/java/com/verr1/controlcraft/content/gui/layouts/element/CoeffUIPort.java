package com.verr1.controlcraft.content.gui.layouts.element;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.gui.layouts.api.LabelProvider;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.links.fma.LinearAdderBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.data.links.Coefficients;
import com.verr1.controlcraft.foundation.data.links.NamedCoeff;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.utils.ParseUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory.title;
import static com.verr1.controlcraft.content.gui.factory.Converter.alignLabel;

public class CoeffUIPort extends ListUIPort<NamedCoeff, Coefficients>{
    private final int max_size = 6;
    private int currentSize = 0;
    private final List<NameCoeffWidget> widgets = ArrayUtils.ListOf(max_size, () -> NameCoeffWidget.create(Minecraft.getInstance().font));

    public CoeffUIPort(BlockPos boundPos) {
        super(
                boundPos,
                LinearAdderBlockEntity.COEFF,
                Coefficients.class,
                Coefficients.EMPTY,
                Coefficients::content,
                Coefficients::new
        );
    }

    @Override
    protected List<NamedCoeff> readList() {
        return IntStream.range(0, currentSize).mapToObj(widgets::get).map(NameCoeffWidget::read).toList();
    }

    @Override
    protected void writeList(List<NamedCoeff> value) {
        currentSize = value.size();
        int size = Math.min(max_size, value.size());
        IntStream.range(0, size).forEach(i -> widgets.get(i).write(value.get(i)));
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        AtomicInteger line = new AtomicInteger(0);
        layoutToFill.addChild(title(UIContents.FMA_COEFFICIENT).toDescriptiveLabel(), line.getAndIncrement(), 0);

        IntStream.range(0, max_size).forEach(i -> {
            layoutToFill.addChild(widgets.get(i).label, line.get(), 0);
            layoutToFill.addChild(widgets.get(i).field, line.getAndIncrement(), 1);
        });
    }

    private void setVisibility(){
        IntStream.range(0, max_size).forEach(i -> widgets.get(i).setVisible(i < currentSize));
    }

    @Override
    public void onMessage(Message msg) {
        if(msg != Message.POST_READ)return;
        setVisibility();
    }

    @Override
    public void onActivatedTab() {
        super.onActivatedTab();
        setVisibility();
    }

    public void alignLabels(){
        alignLabel(widgets.stream().map(w -> (Label)w.label).toList());
    }


    record NameCoeffWidget(FormattedLabel label, EditBox field){
        public static NameCoeffWidget create(Font font){
            return new NameCoeffWidget(new FormattedLabel(0, 0, Component.literal("")),
                    new EditBox(font, 0, 0, 60, 10, Component.literal(""))
            );
        }

        public NamedCoeff read(){
            String name = label.text.getString();
            String value = field.getValue();
            return new NamedCoeff(name, ParseUtils.tryParseDouble(value));
        }

        public void write(NamedCoeff nc){
            label.setText(Component.literal(nc.name()));
            field.setValue("%.4f".formatted(nc.coeff()));
        }

        public void setVisible(boolean visible){
            label.visible = visible;
            field.visible = visible;
        }
    }
}

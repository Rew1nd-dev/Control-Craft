package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.ComponentLike;
import com.verr1.controlcraft.content.gui.widgets.DescriptiveScrollInput;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.SensorTypes;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

public class SensorUIPort extends MultipleTypedUIPort{

    FormattedLabel title = UIContents.SENSOR_SETTINGS.convertTo(Converter::titleStyle).toUILabel();

    FormattedLabel typeTitle  = UIContents.SENSOR_TYPE.convertTo(Converter::titleStyle).toUILabel();
    FormattedLabel localTitle = UIContents.SENSOR_LOCAL.convertTo(Converter::titleStyle).toUILabel();

    FormattedLabel value = new FormattedLabel(0, 0, Component.literal("LLLLL"));

    DescriptiveScrollInput<SensorTypes> options = new DescriptiveScrollInput<>(
            0, 0, 10, 10,
            ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, SensorTypes.class
    );

    SmallCheckbox localCheckbox = new SmallCheckbox(
            0, 0, 10, 10,
            Component.literal(""),
            false
    );

    public SensorUIPort(BlockPos boundPos) {
        super(boundPos, List.of(
                new KeyWithType(SensorBlockEntity.SENSOR, SensorTypes.class, SensorTypes.OMEGA),
                new KeyWithType(SensorBlockEntity.LOCAL, Boolean.class, false)
        ));
        lateInit();
    }

    protected void lateInit(){
        options.valueCalling(
                it -> Optional.of(it)
                        .map(ComponentLike::asComponent)
                        .map(c -> c.copy().withStyle(Converter::optionStyle))
                        .ifPresent(v -> {
                            value.setTextOnly(v);
                            if(it == SensorTypes.ROTATION || it == SensorTypes.EULER_YXZ){
                                localCheckbox.setSelected(false);
                                localCheckbox.visible = false;
                                localTitle.visible = false;
                            }else{
                                localCheckbox.visible = true;
                                localTitle.visible = true;
                            }
                        })
        );
        setMaxLength();
    }

    private void setMaxLength(){
        AtomicInteger maxLen = new AtomicInteger(0);
        options.values().stream().map(ComponentLike::asComponent).forEach(c -> {
            int len = Minecraft.getInstance().font.width(c);
            if(len > maxLen.get()) maxLen.set(len);
        });
        value.setWidth(maxLen.get());

    }

    public void alignLabels(){
        Converter.alignLabel(typeTitle, localTitle);
    }

    @Override
    public void onActivatedTab() {
        alignLabels();
    }

    private SensorTypes valueOfOption(){
        return options.valueOfOption();
    }

    @Override
    protected void writeGUIWithType(List<ValueWithType> vwt) {
        if(vwt.size() != 2){
            throw new IllegalArgumentException("SensorUIPort requires exactly 2 values: SensorTypes and Local");
        }
        options.setToValue(vwt.get(0).cast(SensorTypes.class));
        localCheckbox.setSelected(vwt.get(1).cast(Boolean.class));
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(title, 0, 0);
        layoutToFill.addChild(typeTitle, 1, 0);
        layoutToFill.addChild(value, 1, 1);
        layoutToFill.addChild(options, 1, 2);
        layoutToFill.addChild(localTitle, 2, 0);
        layoutToFill.addChild(localCheckbox, 2, 1);
        layoutToFill.columnSpacing(3).rowSpacing(2);
    }

    @Override
    protected List<Object> readGUI() {
        return List.of(valueOfOption(), localCheckbox.selected());
    }
}

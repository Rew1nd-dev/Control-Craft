package com.verr1.controlcraft.content.gui.layouts.element.general;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.ComponentLike;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.content.gui.layouts.api.LabelProvider;
import com.verr1.controlcraft.content.gui.layouts.api.TitleLabelProvider;
import com.verr1.controlcraft.content.gui.widgets.DescriptiveScrollInput;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class OptionUIField<T extends Enum<?> & Descriptive<?>> extends TypedUIPort<T> implements TitleLabelProvider {



    FormattedLabel title;
    FormattedLabel value = new FormattedLabel(0, 0, Component.literal("LLLLL"));
    DescriptiveScrollInput<T> options;

    List<SwitchListener<T>> onOptionSwitch = new ArrayList<>();

    public OptionUIField(BlockPos boundPos, NetworkKey key, Class<T> clazz, LabelProvider titleText) {
        super(boundPos, key, clazz, tryGetDefault(clazz));
        options = new DescriptiveScrollInput<>(0, 0, 10, 10, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, clazz);
        title = titleText.toDescriptiveLabel();
        lateInit();
    }

    public OptionUIField(BlockPos boundPos, NetworkKey key, Class<T> clazz, T[] scope, LabelProvider titleText) {
        super(boundPos, key, clazz, tryGetDefault(clazz));
        options = new DescriptiveScrollInput<>(0, 0, 10, 10, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, clazz);
        options.withValues(scope);
        title = titleText.toDescriptiveLabel();
        lateInit();
    }

    private static<T> T tryGetDefault(Class<T> clazz){
        try{
            return clazz.getEnumConstants()[0];
        }catch (IndexOutOfBoundsException e){
            ControlCraft.LOGGER.error("class: {} does not contain any enum constant !!!", clazz.getName());
        }
        return null;
    }

    public OptionUIField<T> onOptionSwitch(SwitchListener<T> listener){
        onOptionSwitch.add(listener);
        return this;
    }

    public FormattedLabel valueLabel() {
        return value;
    }

    protected void lateInit(){
        options.valueCalling(
                it -> {
                    Optional.of(it)
                            .map(ComponentLike::asComponent)
                            .map(c -> c.copy().withStyle(Converter::optionStyle))
                            .ifPresent(value::setTextOnly);
                    onOptionSwitch.forEach(o -> o.onSwitch(this, it));
                }
        );
        setMaxLength();
    }

    public FormattedLabel value() {
        return value;
    }

    public DescriptiveScrollInput<T> options() {
        return options;
    }

    public void setMaxLength(){
        AtomicInteger maxLen = new AtomicInteger(0);
        options.values().stream().map(ComponentLike::asComponent).forEach(c -> {
            int len = Minecraft.getInstance().font.width(c);
            if(len > maxLen.get()) maxLen.set(len);
        });
        setMaxLength(maxLen.get());
    }

    public void setMaxLength(int length){
        value.setWidth(length);
    }



    private T valueOfOption(){
        return options.valueOfOption();
    }


    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(title, 0, 0);
        layoutToFill.addChild(value, 0, 1);
        layoutToFill.addChild(options, 0, 2);
        layoutToFill.columnSpacing(3);
    }

    @Override
    protected T readGUI() {
        return valueOfOption();
    }

    @Override
    protected void writeGUI(T value) {
        options.setToValue(value);
    }

    @Override
    public Label title() {
        return title;
    }

    @Override
    public Label[] titles() {
        return new Label[]{title, value};
    }

    @FunctionalInterface
    public interface SwitchListener<T>{
        // void onSwitch(T newValue);
        // onSwitch(newValue);
        void onSwitch(OptionUIField<?> self, T newValue);
    }


}

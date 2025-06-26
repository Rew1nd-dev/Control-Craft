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
import com.verr1.controlcraft.foundation.cimulink.game.port.types.EnumGroup;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GroupEnum;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

public class GroupOptionUIField<G extends Enum<?> & EnumGroup<V> & Descriptive<?>, V extends Enum<?> & GroupEnum<G> & Descriptive<?>> extends TypedUIPort<V> implements TitleLabelProvider {

    FormattedLabel valueTitle;
    FormattedLabel groupTitle;
    FormattedLabel group = new FormattedLabel(0, 0, Component.literal("LLLLL"));
    FormattedLabel value = new FormattedLabel(0, 0, Component.literal("LLLLL"));
    DescriptiveScrollInput<G> groups;
    DescriptiveScrollInput<V> values;

    List<SwitchListener<G, V>> onOptionSwitch = new ArrayList<>();

    Class<V> valueType;
    Class<G> groupType;

    public GroupOptionUIField(
            BlockPos boundPos,
            NetworkKey key,
            Class<V> valueType,
            Class<G> groupType,
            LabelProvider valueTitle,
            LabelProvider groupTitle,
            V defaultValue
    ) {
        super(boundPos, key, valueType, defaultValue);
        this.groupType = groupType;
        this.valueType = valueType;

        this.valueTitle = valueTitle.toDescriptiveLabel();
        this.groupTitle = groupTitle.toDescriptiveLabel();

        values = new DescriptiveScrollInput<>(0, 0, 10, 10, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, defaultValue.group().members());
        groups = new DescriptiveScrollInput<>(0, 0, 10, 10, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, groupType);


        values.valueCalling(
                it -> {
                    Optional.of(it)
                            .map(ComponentLike::asComponent)
                            .map(c -> c.copy().withStyle(Converter::optionStyle))
                            .ifPresent(value::setTextOnly);
                    onOptionSwitch.forEach(o -> o.onSwitch(this, it, it.group()));
                }
        );

        groups.valueCalling(
                it -> {
                    Optional.of(it)
                            .map(ComponentLike::asComponent)
                            .map(c -> c.copy().withStyle(Converter::optionStyle))
                            .ifPresent(group::setTextOnly);

                    V itValue = it.members().length > 0 ? it.members()[0] : null;
                    if(itValue == null){
                        ControlCraft.LOGGER.warn(
                                "Group {} does not have any values, this is illegal",
                                it.name()
                        );
                        return;
                    }
                    values.withValues(it.members()).setToValue(itValue);
                    onOptionSwitch.forEach(o -> o.onSwitch(this, itValue, it));
                }
        );

        setMaxLength();
    }


    public FormattedLabel value() {
        return value;
    }

    public DescriptiveScrollInput<V> options() {
        return values;
    }

    public DescriptiveScrollInput<G> groups() {
        return groups;
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(groupTitle, 0, 0);
        layoutToFill.addChild(group, 0, 1);
        layoutToFill.addChild(groups, 0, 2);
        layoutToFill.addChild(valueTitle, 1, 0);
        layoutToFill.addChild(value, 1, 1);
        layoutToFill.addChild(values, 1, 2);
        layoutToFill.columnSpacing(3).rowSpacing(2);
    }

    @Override
    protected V readGUI() {
        return values.valueOfOption();
    }

    public void setMaxLength(){
        int max_g = MinecraftUtils.maxLength(Arrays.asList(groupType.getEnumConstants()));
        int max_v = MinecraftUtils.maxLength(Arrays.asList(valueType.getEnumConstants()));
        Converter.alignLabel(groupTitle, valueTitle);
        setMaxLength(Math.max(max_g, max_v));
    }

    public void setMaxLength(int length){
        value.setWidth(length);
        group.setWidth(length);

    }

    @Override
    protected void writeGUI(V value) {
        groups.setToValue(value.group());
        values.setToValue(value);
    }

    @Override
    public Label title() {
        return groupTitle;
    }

    @Override
    public Label[] titles() {
        return new Label[]{groupTitle, valueTitle};
    }

    @FunctionalInterface
    public interface SwitchListener<
            G extends Enum<?> & EnumGroup<V> & Descriptive<?>,
            V extends Enum<?> & GroupEnum<G> & Descriptive<?>>
    {
        void onSwitch(GroupOptionUIField<G, V> self, V newValue, G newGroup);
    }

}

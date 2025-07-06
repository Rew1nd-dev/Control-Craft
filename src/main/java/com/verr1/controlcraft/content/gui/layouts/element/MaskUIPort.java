package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.content.links.logic.FlexibleGateBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.FlexibleGateLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.StringBoolean;
import com.verr1.controlcraft.foundation.data.links.StringBooleans;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

public class MaskUIPort extends ListUIPort<StringBoolean, StringBooleans>{

    private final List<MaskUI> lines = ArrayUtils.ListOf(
            FlexibleGateLinkPort.MAX_SIZE,
            $ -> MaskUI.create()
    );
    private int validSize = 0;

    private final FormattedLabel title = UIContents.MASK_IN.convertTo(Converter::titleStyle).toDescriptiveLabel();

    public MaskUIPort(BlockPos boundPos) {
        super(
                boundPos,
                FlexibleGateBlockEntity.STATUS,
                StringBooleans.class,
                StringBooleans.EMPTY,
                StringBooleans::statuses,
                StringBooleans::new
        );
    }

    @Override
    protected List<StringBoolean> readList() {
        return IntStream.range(0, validSize)
                .mapToObj(lines::get)
                .map(MaskUI::read)
                .toList();
    }

    @Override
    protected void writeList(List<StringBoolean> value) {
        validSize = Math.min(value.size(), lines.size());
        for (int i = 0; i < validSize; i++) {
            lines.get(i).write(value.get(i));
        }
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

    public void setVisibility(){
        if(!isActivated)return;
        for (int i = 0; i < lines.size(); i++) {
            boolean isVisible = i < validSize;
            lines.get(i).setVisibility(isVisible, isVisible);
        }
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        layoutToFill.addChild(title, 0, 0, 1, 1);
        for (int i = 0; i < lines.size(); i++) {
            MaskUI line = lines.get(i);
            layoutToFill.addChild(line.name, i + 1, 0);
            layoutToFill.addChild(line.mask, i + 1, 1);
        }
        layoutToFill.rowSpacing(2).columnSpacing(2);
    }

    record MaskUI(FormattedLabel name, SmallCheckbox mask){

        public static MaskUI create(){
            return new MaskUI(
                    new FormattedLabel(),
                    new SmallCheckbox()
            );
        }

        public StringBoolean read(){
            return new StringBoolean(name.text.getString(), mask.selected());
        }

        public void write(StringBoolean value) {
            name.setTextOnly(Converter.convert(Converter::optionStyle, Component.literal(value.name())));
            mask.setSelected(value.enabled());
        }

        public void setVisibility(boolean name, boolean mask){
            this.name.visible = name;
            this.mask.visible = mask;
        }
    }

}

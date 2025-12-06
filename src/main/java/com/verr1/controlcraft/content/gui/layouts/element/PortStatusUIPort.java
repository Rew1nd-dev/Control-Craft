package com.verr1.controlcraft.content.gui.layouts.element;

import com.simibubi.create.foundation.gui.widget.Label;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.IconSelectionScrollInput;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.data.links.StringBoolean;
import com.verr1.controlcraft.foundation.data.links.StringBooleans;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory.title;
import static com.verr1.controlcraft.content.gui.factory.Converter.alignLabel;

public class PortStatusUIPort extends ListUIPort<StringBoolean, StringBooleans>{
    private final int max_size = 28;
    private final int max_page_size = 14;
    private final IconSelectionScrollInput pager = new IconSelectionScrollInput(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION_PRESSED);
    private int currentSize = 0;
    private int currentPage = 0;
    private final List<NameEnableWidget> widgets =
            ArrayUtils.ListOf(max_size, () -> NameEnableWidget.create(Minecraft.getInstance().font));

    private final FormattedLabel label;

    public PortStatusUIPort(
            BlockPos boundPos
    ) {
        super(
                boundPos,
                ProxyLinkBlockEntity.ALL_STATUS,
                StringBooleans.class,
                StringBooleans.EMPTY,
                StringBooleans::statuses,
                StringBooleans::new
        );
        label = title(UIContents.STATUS).toDescriptiveLabel();
        List<Component> pageComponents = IntStream.range(0, max_size / max_page_size)
                .mapToObj(i -> (Component)Component.literal(String.valueOf(i + 1))).toList();
        pager.forOptions(pageComponents)
                .calling(i -> {
                    currentPage = i;
                    setVisibility();
                });
    }

    @Override
    protected List<StringBoolean> readList() {
        return IntStream.range(0, currentSize).mapToObj(widgets::get).map(NameEnableWidget::read).toList();
    }

    @Override
    protected void writeList(List<StringBoolean> value) {
        currentSize = value.size();
        int size = Math.min(max_size, value.size());
        IntStream.range(0, size).forEach(i -> widgets.get(i).write(value.get(i)));
        redoLayout();
    }

    @Override
    public void initLayout(GridLayout layoutToFill) {
        AtomicInteger line = new AtomicInteger(0);
        GridLayout grid = new GridLayout();
        layoutToFill.addChild(label, 0, 0, 1, 1);

        IntStream.range(0, max_size).forEach(i -> {
            grid.addChild(widgets.get(i).label, line.get() % 7, (2 * (line.get() / 7)) % 4);
            grid.addChild(widgets.get(i).field, line.get() % 7, (2 * (line.get() / 7) + 1) % 4);
            line.getAndIncrement();
        });
        grid.columnSpacing(2).rowSpacing(2);
        layoutToFill.addChild(grid, 1, 0, 1, 3);
        layoutToFill.addChild(pager, 2, 0);
        layoutToFill.columnSpacing(4).rowSpacing(2);
    }

    private boolean insidePage(int index){
        return index >= currentPage * max_page_size && index < (currentPage + 1) * max_page_size;
    }

    private void setVisibility(){
        IntStream.range(0, max_size).forEach(i -> widgets.get(i).setVisible(i < currentSize && insidePage(i)));
        pager.visible = currentSize > max_page_size;
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

    record NameEnableWidget(FormattedLabel label, SmallCheckbox field){
        public static NameEnableWidget create(Font font){
            return new NameEnableWidget(new FormattedLabel(0, 0, Component.literal("")),
                    new SmallCheckbox(0, 0, 10, 10, Component.literal(""), false)
            );
        }


        public StringBoolean read(){
            String name = label.text.getString();
            boolean value = field.selected();
            return new StringBoolean(name, value);
        }

        public void write(StringBoolean ps){
            label.setText(Component.literal(ps.name()).withStyle(Converter::optionStyle));
            field.setSelected(ps.enabled());
        }

        public void setVisible(boolean visible){
            label.visible = visible;
            field.visible = visible;
        }
    }
}

package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.content.gui.screens.CircuitWirelessScreen;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.IconSelectionScrollInput;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.content.links.circuit.CircuitBlockEntity;
import com.verr1.controlcraft.content.links.circuit.IoData;
import com.verr1.controlcraft.content.links.circuit.IoSettings;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.data.links.CircuitPortStatus;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.ParseUtils;
import kotlin.Pair;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.IntStream;

import static com.verr1.controlcraft.content.gui.factory.Converter.alignLabel;

public class CircuitUIPort extends TypedUIPort<CompoundTag> {


    private final List<MutableBlock> data = new ArrayList<>();
    private final List<CircuitPortStatus> immutableData = new ArrayList<>();
    private final BlockUI block = new BlockUI(Minecraft.getInstance().font);
    private int currentIndex = 0;
    private final IconSelectionScrollInput blockSelector = (IconSelectionScrollInput)
            new IconSelectionScrollInput(
                    0, 0, 10, 10,
                    ControlCraftGuiTextures.SMALL_BUTTON_SELECTION
            ).calling(this::setBlock);

    private GridLayout blockLayout = new GridLayout();

    public CircuitUIPort(BlockPos boundPos) {
        super(boundPos, CircuitBlockEntity.CIRCUIT, CompoundTag.class, new CompoundTag());
    }



    @Override
    protected void initLayout(GridLayout layoutToFill) {

        blockLayout = block.createLayout();
        // Add BlockUI and block selector to layout
        layoutToFill.addChild(blockLayout, 0, 0);
        layoutToFill.addChild(blockSelector, 1, 0);
        layoutToFill.rowSpacing(8);
    }

    private void setBlock(int index) {
        if (!validIndex(index)) return;
        writeCurrent();
        read(index);
        currentIndex = index;
    }

    private boolean validIndex(int index) {
        return index >= 0 && index < data.size();
    }

    private void writeCurrent() {
        if (validIndex(currentIndex)) block.write(data.get(currentIndex));
    }

    private void read(int index) {
        block.read(data.get(index));
    }

    @Override
    protected CompoundTag readGUI() {
        writeCurrent();

        List<CircuitPortStatus> input = data
                .stream().flatMap(d -> d.lines.stream().filter(l -> l.isInput))
                .map(MutableLine::immutable)
                .toList();

        List<CircuitPortStatus> output = data
                .stream().flatMap(d -> d.lines.stream().filter(l -> !l.isInput))
                .map(MutableLine::immutable)
                .toList();

        return CircuitBlockEntity.PAIR_SER.serialize(new Pair<>(input, output));
    }

    @Override
    protected void writeGUI(CompoundTag value) {

        var inputStatus = CircuitBlockEntity.PAIR_SER.deserialize(value);

        List<CircuitPortStatus> newData = ArrayUtils.flatten(inputStatus.getFirst(), inputStatus.getSecond());

        immutableData.clear();
        immutableData.addAll(newData);

        data.clear();

        int pages = (immutableData.size() - 1) / MAX_LINE_PER_PAGE + 1;
        for (int i = 0; i < pages; i++) {
            int start = i * MAX_LINE_PER_PAGE;
            int end = Math.min(start + MAX_LINE_PER_PAGE, immutableData.size());
            data.add(new MutableBlock(immutableData.subList(start, end)));
        }

        int labelMaxLen = MinecraftUtils.maxTitleLength(immutableData.stream().map(CircuitPortStatus::portName).toList());

        // Initialize BlockUI

        block.setLabelWidth(labelMaxLen);
        blockLayout.arrangeElements();

        // Configure block selector
        blockSelector.forOptions(
                IntStream.range(0, pages)
                        .mapToObj(i -> Component.literal("Page " + (i + 1)))
                        .toList()
        );

        read(0);
        currentIndex = 0; // don't write
        blockSelector.setState(0);
    }






    private void setVisibility(){
        // IntStream.range(0, max_size).forEach(i -> widgets.get(i).setVisible(i < currentSize));
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



    public static final int MAX_LINE_PER_PAGE = 6;


    static class MutableLine{
        double value;
        boolean enabled;
        boolean isInput;
        final String name;


        MutableLine(CircuitPortStatus immutableData) {
            value = immutableData.value();
            enabled = immutableData.enabled();
            isInput = immutableData.isInput();
            name = immutableData.portName();
        }

        public CircuitPortStatus immutable(){
            return new CircuitPortStatus(name, value, isInput, enabled);
        }

    }

    static class MutableBlock{
        List<MutableLine> lines = new ArrayList<>();

        public MutableBlock(List<CircuitPortStatus> blockData) {
            for(CircuitPortStatus line : blockData){
                lines.add(new MutableLine(line));
            }
        }
    }



    static class LineUI {

        final FormattedLabel label;
        final EditBox value;
        final SmallCheckbox field;
        final AtomicBoolean isInput;

        public LineUI(@NotNull Font font) {
            int input_len_x = 50;
            int len_y = 10;

            label = new FormattedLabel(0, 0, Component.literal(""));
            value = new EditBox(font, 0, 0, input_len_x, len_y, Component.literal(""));
            field = new SmallCheckbox(0, 0, len_y, len_y, Component.literal(""), false);
            isInput = new AtomicBoolean(false);
        }

        public void setLabelWidth(int width){
            label.setWidth(width);
        }


        public void read(MutableLine line){
            label.setTextOnly(Component.literal(line.name).withStyle(s -> line.isInput
                    ?
                    s.withColor(ChatFormatting.DARK_AQUA)
                    :
                    s.withColor(ChatFormatting.DARK_BLUE))
            );
            field.setSelected(line.enabled);
            value.setValue(String.format("%.4f", line.value));
            value.setEditable(line.isInput);
            isInput.set(line.isInput);
        }

        public void write(MutableLine data){
            data.value = ParseUtils.tryParseDouble(value.getValue());
            data.enabled = field.selected();
            data.isInput = isInput.get();
        }

        public void setVisible(boolean vl, boolean vf, boolean vv){
            label.visible = vl;
            field.visible = vf;
            value.visible = vv;
        }

        public GridLayout createLayout(){
            GridLayout layout = new GridLayout();
            layout.addChild(label, 0, 0);
            layout.addChild(value, 0, 1);
            layout.addChild(field, 0, 2);
            layout.columnSpacing(2);
            return layout;
        }

    }

    static class BlockUI {

        private final List<LineUI> lines = new ArrayList<>();
        private int currentValidSize = 0;

        public BlockUI(@NotNull Font font) {
            for (int i = 0; i < CircuitUIPort.MAX_LINE_PER_PAGE; i++) {
                lines.add(new LineUI(font));
            }
        }

        public void setLabelWidth(int maxLen){
            lines.forEach(l -> l.setLabelWidth(maxLen));
        }

        public void read(MutableBlock data) {
            int lineCount = Math.min(lines.size(), data.lines.size());
            for (int i = 0; i < lineCount; i++) {
                lines.get(i).read(data.lines.get(i));
                lines.get(i).setVisible(true, true, true);
            }
            currentValidSize = lineCount;
            for (int i = lineCount; i < lines.size(); i++) {
                lines.get(i).setVisible(false, false, false);
            }
        }

        public void write(MutableBlock data) {
            int lineCount = Math.min(lines.size(), data.lines.size());
            for (int i = 0; i < lineCount; i++) {
                lines.get(i).write(data.lines.get(i));
            }
        }

        public int size(){
            return currentValidSize;
        }

        public GridLayout createLayout() {
            GridLayout blockLayout = new GridLayout();
            for (int i = 0; i < lines.size(); i++) {
                blockLayout.addChild(lines.get(i).createLayout(), i, 0);
            }
            blockLayout.rowSpacing(1);
            return blockLayout;
        }
    }
}

package com.verr1.controlcraft.content.gui.screens;

import com.simibubi.create.foundation.gui.AllGuiTextures;
import com.simibubi.create.foundation.gui.menu.AbstractSimiContainerScreen;
import com.simibubi.create.foundation.utility.Components;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.IconSelectionScrollInput;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.content.links.circuit.IoData;
import com.verr1.controlcraft.content.links.circuit.IoSettings;
import com.verr1.controlcraft.foundation.cimulink.game.misc.CircuitWirelessMenu;
import com.verr1.controlcraft.foundation.data.terminal.TerminalRowSetting;
import com.verr1.controlcraft.foundation.network.packets.specific.CircuitSettingsPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.TerminalSettingsPacket;
import com.verr1.controlcraft.foundation.type.descriptive.MiscDescription;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.ParseUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static com.simibubi.create.foundation.gui.AllGuiTextures.PLAYER_INVENTORY;

public class CircuitWirelessScreen extends AbstractSimiContainerScreen<CircuitWirelessMenu> {

    public static final int LINE_BLOCK = 6;


    static class MutableLine{
        double min;
        double max;
        boolean enabled;
        boolean isInput;
        final String name;


        MutableLine(IoData immutableData) {
            min = immutableData.min();
            max = immutableData.max();
            enabled = immutableData.enabled();
            isInput = immutableData.isInput();
            name = immutableData.ioName();
        }

        public IoSettings immutable(){
            return new IoSettings(min, max, enabled);
        }

    }

    static class MutableBlock{
        List<MutableLine> lines = new ArrayList<>();

        public MutableBlock(List<IoData> blockData) {
            for(IoData line : blockData){
                lines.add(new MutableLine(line));
            }
        }


    }

    static class LineUI {

        FormattedLabel name;
        FormattedLabel minTitle;
        FormattedLabel maxTitle;
        EditBox minField;
        EditBox maxField;
        SmallCheckbox toggleField;
        final int labelMaxLen;

        public LineUI(@NotNull Font font, int labelMaxLen) {
            this.labelMaxLen = labelMaxLen;

            int input_len_x = 30;
            int len_y = 10;

            name = UIContents.NAME.toUILabel(); // will be modified
            minTitle = UIContents.MIN.toDescriptiveLabel();
            minField = new EditBox(font, 0, 0, input_len_x, len_y, Components.literal(""));
            maxTitle = UIContents.MAX.toDescriptiveLabel();
            maxField = new EditBox(font, 0, 0, input_len_x, len_y, Components.literal(""));
            toggleField = new SmallCheckbox(0, 0, 10, 10, MiscDescription.TURN_ON.specific().get(0), false);

            minField.setFilter(ParseUtils::tryParseDoubleFilter);
            maxField.setFilter(ParseUtils::tryParseDoubleFilter);
            name.setWidth(labelMaxLen);
        }

        public void read(MutableLine data){

            name.setTextOnly(Component.literal(data.name).withStyle(s -> data.isInput
                    ?
                    s.withColor(ChatFormatting.DARK_AQUA)
                    :
                    s.withColor(ChatFormatting.DARK_BLUE))
            );

            minField.setValue(String.format("%.2f", data.min));
            maxField.setValue(String.format("%.2f", data.max));
            toggleField.setSelected(data.enabled);


            setVisible(false, true, true);

        }

        public void write(MutableLine data){
            data.min = ParseUtils.tryParseDouble(minField.getValue());
            data.max = ParseUtils.tryParseDouble(maxField.getValue());
            data.enabled = toggleField.selected();
        }



        public void setVisible(boolean field, boolean nameF, boolean toggle){
            minField.visible = field;
            maxField.visible = field;
            minTitle.visible = field;
            maxTitle.visible = field;
            toggleField.visible = toggle;
            name.visible = nameF;
        }



        public GridLayout createUI(){
            GridLayout lineLayout = new GridLayout();
            lineLayout.addChild(name, 0, 0);
            lineLayout.addChild(minTitle, 0, 1);
            lineLayout.addChild(minField, 0, 2);
            lineLayout.addChild(maxTitle, 0, 3);
            lineLayout.addChild(maxField, 0, 4);
            lineLayout.addChild(toggleField, 0, 5);
            lineLayout.columnSpacing(6);
            return lineLayout;
        }
    }

    static class BlockUI{
        private final List<LineUI> lines = new ArrayList<>();
        private int currentValidSize = 0;

        BlockUI(@NotNull Font font, int labelMaxLen){
            for(int i = 0; i < LINE_BLOCK; i++){
                lines.add(new LineUI(font, labelMaxLen));
            }
        }

        public void read(MutableBlock data){
            int lineCount = Math.min(lines.size(), data.lines.size());
            for(int i = 0; i < lineCount; i++){
                lines.get(i).read(data.lines.get(i));
            }
            currentValidSize = lineCount;
            for (int i = lineCount; i < lines.size(); i++){
                lines.get(i).setVisible(false, false, false);
            }
        }

        public int size(){
            return currentValidSize;
        }

        public void write(MutableBlock data){
            int lineCount = Math.min(lines.size(), data.lines.size());
            for(int i = 0; i < lineCount; i++){
                lines.get(i).write(data.lines.get(i));
            }
        }


        public GridLayout createLayout(){
            GridLayout blockLayout = new GridLayout();
            for(int i = 0; i < lines.size(); i++){
                blockLayout.addChild(lines.get(i).createUI(), i, 0);
            }
            blockLayout.rowSpacing(8);
            return blockLayout;
        }

    }

    private final List<IoData> immutableData;
    private final ControlCraftGuiTextures background = ControlCraftGuiTextures.SIMPLE_BACKGROUND_LARGE;
    private final AllGuiTextures slot = AllGuiTextures.JEI_SLOT;



    private final BlockPos pos;


    private final List<MutableBlock> data = new ArrayList<>();

    private final BlockUI block;


    private final GridLayout layout = new GridLayout();

    private int currentIndex = 0;

    private final IconSelectionScrollInput blockSelector = (IconSelectionScrollInput)
            new IconSelectionScrollInput(
                    0, 0, 10, 10,
                    ControlCraftGuiTextures.SMALL_BUTTON_SELECTION
            ).calling(this::setBlock);


    public CircuitWirelessScreen(CircuitWirelessMenu container, Inventory inv, Component title) {
        super(container, inv, title);

        this.pos = menu.contentHolder.pos;
        this.immutableData = menu.contentHolder.ioDatas;
        int initialSize = immutableData.size();

        int labelMaxLen = MinecraftUtils.maxTitleLength(immutableData.stream().map(IoData::ioName).toList());


        block = new BlockUI(Minecraft.getInstance().font, labelMaxLen);

        int pages = (initialSize - 1) / LINE_BLOCK + 1;
        for (int i = 0; i < pages; i++) {
            int start = i * LINE_BLOCK;
            int end = Math.min(start + LINE_BLOCK, initialSize);
            data.add(new MutableBlock(immutableData.subList(start, end)));
        }

        blockSelector.forOptions(
                IntStream.range(0, pages)
                        .mapToObj(i -> Components.literal("Page " + (i + 1)))
                        .toList()
        );

        read(0);
        currentIndex = 0;
        menu.setPage(0, data.get(0).lines.size());

    }

    @Override
    public void init(){
        setWindowSize(
                Math.max(
                        background.width,
                        PLAYER_INVENTORY.width
                ),
                background.height + 4 + PLAYER_INVENTORY.height
        );
        super.init();


        layout.addChild(block.createLayout(), 0, 0);
        layout.addChild(blockSelector, 1, 0);
        layout.setX(leftPos + 8 + 40);
        layout.setY(topPos + 8 + 4);

        layout.visitWidgets(this::addRenderableWidget);
        layout.arrangeElements();

    }

    private void setBlock(int index){
        if(!validIndex(index))return;
        writeCurrent();
        read(index);
        currentIndex = index;
        menu.setPage(index, data.get(index).lines.size());
    }

    private boolean validIndex(int index){
        return index >= 0 && index < data.size();
    }

    private void writeCurrent(){
        if(validIndex(currentIndex)) block.write(data.get(currentIndex));
    }

    private void read(int index){
        block.read(data.get(index));
    }

    @Override
    public void onClose() {
        super.onClose();
        confirm();
    }



    public void confirm(){
        writeCurrent();
        List<IoSettings> newSettings = new ArrayList<>();
        for (MutableBlock block : data) {
            for (int j = 0; j < block.lines.size(); j++) {
                MutableLine line = block.lines.get(j);
                newSettings.add(line.immutable());
            }
        }
        var packet = new CircuitSettingsPacket(pos, newSettings);
        ControlCraftPackets.getChannel().sendToServer(packet);
    }

    @Override
    protected void renderBg(@NotNull GuiGraphics graphics, float p_97788_, int p_97789_, int p_97790_) {
        int invX = getLeftOfCentered(PLAYER_INVENTORY.width);
        int invY = topPos + background.height + 4;
        renderPlayerInventory(graphics, invX, invY);

        int x = leftPos;
        int y = topPos;

        background.render(graphics, x, y);
        renderFrequencySlot(graphics);
    }

    private void renderFrequencySlot(GuiGraphics graphics){
        int x = leftPos + 8;
        int y = topPos + 8;

        for(int row = 0; row < block.size(); row++){
            for(int column = 0; column < 2; column++){
                slot.render(graphics, x + column * 18, y + row * 18);
            }
        }
    }
}

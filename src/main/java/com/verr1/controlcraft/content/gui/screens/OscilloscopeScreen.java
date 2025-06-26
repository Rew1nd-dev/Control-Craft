package com.verr1.controlcraft.content.gui.screens;

import com.simibubi.create.foundation.gui.AbstractSimiScreen;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.gui.layouts.api.SizedScreenElement;
import com.verr1.controlcraft.content.gui.layouts.element.general.UnitUIPanel;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.MultiPlotWidget;
import com.verr1.controlcraft.content.gui.widgets.SmallCheckbox;
import com.verr1.controlcraft.content.gui.widgets.SmallIconButton;
import com.verr1.controlcraft.content.links.scope.OscilloscopeBlockEntity;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.Optional;
import java.util.stream.Stream;

import static com.verr1.controlcraft.content.links.scope.OscilloscopeBlockEntity.Plots.PLOT_SIZE;

@OnlyIn(Dist.CLIENT)
public class OscilloscopeScreen extends AbstractSimiScreen {

    private final SmallIconButton addChannelButton;
    private final SmallIconButton removeChannelButton;
    private final FormattedLabel addChannelLabel = Converter.convert(UIContents.FMA_INC, Converter::titleStyle).toUILabel();
    private final FormattedLabel removeChannelLabel = Converter.convert(UIContents.FMA_DEC, Converter::titleStyle).toUILabel();
    private final MultiPlotWidget plotWidget;
    private final BlockPos boundPos;
    private final SizedScreenElement background = ControlCraftGuiTextures.SIMPLE_BACKGROUND_LARGE;

    private final SizedScreenElement scopeBackground = ControlCraftGuiTextures.SIMPLE_BACKGROUND_QUARTER;

    private final SmallIconButton autoFit = new SmallIconButton(0, 0, ControlCraftGuiTextures.SMALL_BUTTON_YES).withCallback(this::fit); // Converter.convert(UIContents.PLACE_HOLDER, Converter::titleStyle).toUILabel();



    public OscilloscopeScreen(BlockPos boundPos) {
        addChannelButton = new SmallIconButton(0, 0, ControlCraftGuiTextures.SMALL_BUTTON_YES).withCallback(this::addChannel);
        removeChannelButton = new SmallIconButton(0, 0, ControlCraftGuiTextures.SMALL_BUTTON_YES).withCallback(this::removeChannel);
        this.boundPos = boundPos;
        this.plotWidget = new MultiPlotWidget(Minecraft.getInstance().font, 0, 0, 180, 75, new MultiPlotWidget.ChannelDataSupplier() {
            @Override
            public int size() {
                return channelSize();
            }

            @Override
            public double span() {
                return OscilloscopeScreen.this.span();
            }

            @Override
            public Stream<Double> fetch(int channel) {
                return boundBlockEntity().map(be -> be.clientReceivedData.plots.get(channel).stream()).orElse(Stream.empty());
            }
        });
    }

    public void fit(){
        plotWidget.fit();
    }

    public double span(){
        return PLOT_SIZE * 0.05;
    }

    public int channelSize(){
        return boundBlockEntity().map(be -> be.clientReceivedData.plots.size()).orElse(0);
    }

    private Optional<OscilloscopeBlockEntity> boundBlockEntity(){
        return GenericUIFactory.boundBlockEntity(boundPos, OscilloscopeBlockEntity.class);
    }

    public void addChannel(){
        boundBlockEntity()
                .ifPresent(blockEntity -> {
                    blockEntity.panel().request(0.0, boundPos, OscilloscopeBlockEntity.ADD_CHANNEL);
                });
    }

    public void removeChannel(){
        boundBlockEntity()
                .ifPresent(blockEntity -> {
                    blockEntity.panel().request(0.0, boundPos, OscilloscopeBlockEntity.REMOVE_CHANNEL);
                });
    }

    @Override
    protected void init() {
        setWindowSize(background.width(), background.height());
        super.init();
        GridLayout addRet = new GridLayout();
        addRet.addChild(addChannelLabel, 0, 0);
        addRet.addChild(addChannelButton, 0, 1);
        addRet.addChild(removeChannelLabel, 0, 2);
        addRet.addChild(removeChannelButton, 0, 3);
        addRet.columnSpacing(2);
        GridLayout total = new GridLayout();

        GridLayout controlButtonLayout = new GridLayout();
        controlButtonLayout.addChild(autoFit, 0, 0);
        controlButtonLayout.addChild(plotWidget, 1, 1, 1, 10);
        controlButtonLayout.rowSpacing(6).columnSpacing(2);

        total.addChild(addRet, 0, 0);
        total.addChild(controlButtonLayout, 1, 0);

        total.rowSpacing(0);

        total.setPosition(guiLeft + 24, guiTop + 6);
        total.arrangeElements();

        addRenderableWidgets(
                addChannelButton,
                removeChannelButton,
                addChannelLabel,
                removeChannelLabel,
                plotWidget,
                autoFit
        );

        plotWidget.fit();
    }

    @Override
    protected void renderWindow(GuiGraphics graphics, int mouseX, int mouseY, float partialTicks) {
        background.render(graphics, guiLeft, guiTop);
    }
}

package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.IconSelectionScrollInput;
import com.verr1.controlcraft.content.gui.widgets.ListSelectionScrollInput;
import com.verr1.controlcraft.content.gui.widgets.SmallIconButton;
import com.verr1.controlcraft.content.links.connector.EasyConnectorBlockEntity;
import com.verr1.controlcraft.foundation.api.delegate.IRemoteDevice;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.network.remote.RemotePanel;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory.title;
import static com.verr1.controlcraft.content.gui.factory.GenericUIFactory.boundBlockEntity;

public class ConnectionStatusUIPort extends TypedUIPort<EasyConnectorBlockEntity.Status> {

    public final BlockPos pos;

    private final FormattedLabel nameViewIn = new FormattedLabel(10, 10, Component.literal(""));
    private final FormattedLabel portViewIn = new FormattedLabel(10, 10, Component.literal(""));;
    private final ListSelectionScrollInput<WorldBlockPos> availNamesIn = new ListSelectionScrollInput<>(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION);
    private final ListSelectionScrollInput<String> availPortsIn = new ListSelectionScrollInput<>(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION);


    private final FormattedLabel nameViewOut = new FormattedLabel(10, 10, Component.literal(""));
    private final FormattedLabel portViewOut = new FormattedLabel(10, 10, Component.literal(""));;
    private final ListSelectionScrollInput<WorldBlockPos> availNamesOut = new ListSelectionScrollInput<>(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION);
    private final ListSelectionScrollInput<String> availPortsOut = new ListSelectionScrollInput<>(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION);


    private final FormattedLabel addLabel = title(UIContents.ADD_PORT_TO_BUS).toUILabel();
    private final FormattedLabel retLabel = title(UIContents.RET_PORT_FROM_BUS).toUILabel();
    private final SmallIconButton add = new SmallIconButton(ControlCraftGuiTextures.SMALL_BUTTON_YES).withCallback(this::add);
    private final SmallIconButton ret = new SmallIconButton(ControlCraftGuiTextures.SMALL_BUTTON_NO).withCallback(this::ret);

    private final FormattedLabel ins = title(UIContents.AVAILABLE_IN_PORTS).toDescriptiveLabel();
    private final FormattedLabel outs = title(UIContents.AVAILABLE_OUT_PORTS).toUILabel();

    public ConnectionStatusUIPort(
            BlockPos boundPos
    ) {
        super(boundPos, EasyConnectorBlockEntity.INFO, EasyConnectorBlockEntity.Status.class, EasyConnectorBlockEntity.Status.EMPTY);
        this.pos = boundPos;
    }


    private static void makeScrollWidget(
            List<WorldBlockPos> main,
            Function<WorldBlockPos, String> mainToName,
            Function<WorldBlockPos, List<String>> mainToSub,
            FormattedLabel mainView, FormattedLabel subView,
            ListSelectionScrollInput<WorldBlockPos> mainScroll, ListSelectionScrollInput<String> subScroll
    ){
        mainScroll
                .forOptions(main, wbp -> Component.literal(mainToName.apply(wbp)))
                .calling(i -> {
                    if(i >= main.size())return;
                    String mainName = mainToName.apply(main.get(i));
                    List<String> subs = mainToSub.apply(main.get(i));
                    mainView.setTextOnly(Component.literal(mainName).withStyle(Converter::optionStyle));

                    subScroll
                            .forOptions(subs, Component::literal)
                            .calling(j -> {
                                if(j >= subs.size())return;
                                subView.setTextOnly(Component.literal(subs.get(j)).withStyle(Converter::optionStyle));
                            })
                            .setState(0)
                            .onChanged();
                })
                .setState(0)
                .onChanged();

    }

    private EasyConnectorBlockEntity.ConnectionStatus currentRequest(){
        return new EasyConnectorBlockEntity.ConnectionStatus(
                availNamesOut.currentOption(),
                availNamesIn.currentOption(),
                availPortsOut.currentOption(),
                availPortsIn.currentOption()
        );
    }

    private Optional<RemotePanel> panel(){
        return boundBlockEntity(pos, IRemoteDevice.class).map(IRemoteDevice::panel);
    }

    private void add(){
        panel().ifPresent(p -> p.request(currentRequest(), pos, EasyConnectorBlockEntity.CONNECT));
    }

    private void ret(){
        panel().ifPresent(p -> p.request(currentRequest(), pos, EasyConnectorBlockEntity.DISCONNECT));
    }

    @Override
    protected void initLayout(GridLayout layoutToFill) {
        int line = 0;
        GridLayout operationLayout = new GridLayout();
        operationLayout.addChild(addLabel, line, 0);
        operationLayout.addChild(add, line, 1);
        operationLayout.addChild(retLabel, line, 2);
        operationLayout.addChild(ret, line, 3);
        operationLayout.columnSpacing(2).rowSpacing(2);

        layoutToFill.addChild(outs, line, 0, 1, 5);
        line++;
        layoutToFill.addChild(nameViewOut, line, 0);
        layoutToFill.addChild(availNamesOut, line, 1);
        layoutToFill.addChild(portViewOut, line, 2);
        layoutToFill.addChild(availPortsOut, line, 3);
        line++;
        layoutToFill.addChild(ins, line, 0, 1, 5);
        line++;
        layoutToFill.addChild(nameViewIn, line, 0);
        layoutToFill.addChild(availNamesIn, line, 1);
        layoutToFill.addChild(portViewIn, line, 2);
        layoutToFill.addChild(availPortsIn, line, 3);
        line++;
        layoutToFill.addChild(operationLayout, line, 0, 1, 5);
        layoutToFill.rowSpacing(2).columnSpacing(2);

    }

    @Override
    protected EasyConnectorBlockEntity.Status readGUI() {
        return EasyConnectorBlockEntity.Status.EMPTY;
    }

    @Override
    protected void writeGUI(EasyConnectorBlockEntity.Status value) {
        List<String> allNames = value.allCimulinks().stream().map(EasyConnectorBlockEntity.LinkStatus::name).distinct().toList();
        List<String> allPortName = value.allCimulinks().stream().flatMap(c -> ArrayUtils.flatten(c.inputNames(), c.outputNames()).stream()).distinct().toList();

        int maxName = MinecraftUtils.maxTitleLength(allNames);
        int maxPortName = MinecraftUtils.maxTitleLength(allPortName);

        nameViewIn.setWidth(maxName);
        nameViewOut.setWidth(maxName);
        portViewIn.setWidth(maxPortName);
        portViewOut.setWidth(maxPortName);

        List<WorldBlockPos> wbp = value.allCimulinks().stream()
                .sorted(Comparator.comparing(EasyConnectorBlockEntity.LinkStatus::name))
                .map(EasyConnectorBlockEntity.LinkStatus::pos)
                .toList();

        Map<WorldBlockPos, String> mainToName = value.allCimulinks().stream()
                .collect(Collectors.toMap(
                        EasyConnectorBlockEntity.LinkStatus::pos,
                        EasyConnectorBlockEntity.LinkStatus::name
                ));

        Map<WorldBlockPos, List<String>> mainToIn = value.allCimulinks().stream()
                .collect(Collectors.toMap(
                        EasyConnectorBlockEntity.LinkStatus::pos,
                        EasyConnectorBlockEntity.LinkStatus::inputNames
                ));

        Map<WorldBlockPos, List<String>> mainToOut = value.allCimulinks().stream()
                .collect(Collectors.toMap(
                        EasyConnectorBlockEntity.LinkStatus::pos,
                        EasyConnectorBlockEntity.LinkStatus::outputNames
                ));



        makeScrollWidget(
                wbp,
                arg -> mainToName.getOrDefault(arg, "--"),
                arg -> mainToOut.getOrDefault(arg, List.of()),
                nameViewOut, portViewOut,
                availNamesOut, availPortsOut
        );

        makeScrollWidget(
                wbp,
                arg -> mainToName.getOrDefault(arg, "--"),
                arg -> mainToIn.getOrDefault(arg, List.of()),
                nameViewIn, portViewIn,
                availNamesIn, availPortsIn
        );
        redoLayout();
    }
}

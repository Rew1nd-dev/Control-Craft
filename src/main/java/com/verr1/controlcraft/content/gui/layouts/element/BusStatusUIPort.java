package com.verr1.controlcraft.content.gui.layouts.element;

import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.content.gui.widgets.FormattedLabel;
import com.verr1.controlcraft.content.gui.widgets.IconSelectionScrollInput;
import com.verr1.controlcraft.content.gui.widgets.SmallIconButton;
import com.verr1.controlcraft.content.links.bus.BusBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.BusLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.BusPort;
import com.verr1.controlcraft.foundation.type.descriptive.UIContents;
import com.verr1.controlcraft.registry.ControlCraftGuiTextures;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;

import java.util.*;

import static com.verr1.controlcraft.content.gui.factory.CimulinkUIFactory.title;

public class BusStatusUIPort extends TypedUIPort<BusLinkPort.Status> {

    private final FormattedLabel available = title(UIContents.AVAILABLE_PORTS).toDescriptiveLabel();
    private final FormattedLabel exposed = title(UIContents.EXPOSED_PORTS).toUILabel();
    private final FormattedLabel addLabel = title(UIContents.ADD_PORT_TO_BUS).toUILabel();
    private final FormattedLabel retLabel = title(UIContents.RET_PORT_FROM_BUS).toUILabel();

    private final FormattedLabel availNameView = new FormattedLabel(10, 10, Component.literal(""));
    private final FormattedLabel availPortView = new FormattedLabel(10, 10, Component.literal(""));

    private final IconSelectionScrollInput availNames = new IconSelectionScrollInput(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION_PRESSED);
    private final IconSelectionScrollInput availPorts = new IconSelectionScrollInput(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION, ControlCraftGuiTextures.SMALL_BUTTON_SELECTION_PRESSED);


    private final FormattedLabel usedNamePortView = new FormattedLabel(10, 10, Component.literal(""));

    private final IconSelectionScrollInput usedNamePort = new IconSelectionScrollInput(ControlCraftGuiTextures.SMALL_BUTTON_SELECTION,ControlCraftGuiTextures.SMALL_BUTTON_SELECTION_PRESSED);

    private final SmallIconButton add = new SmallIconButton(ControlCraftGuiTextures.SMALL_BUTTON_YES, ControlCraftGuiTextures.SMALL_BUTTON_YES_PRESSED).withCallback(this::addToUsed);
    private final SmallIconButton ret = new SmallIconButton(ControlCraftGuiTextures.SMALL_BUTTON_NO, ControlCraftGuiTextures.SMALL_BUTTON_NO_PRESSED).withCallback(this::removeUsed);

    final List<String> currentIn = new ArrayList<>();
    final List<String> currentOut = new ArrayList<>();
    final Map<String, Set<String>> availableIn = new HashMap<>();
    final Map<String, Set<String>> availableOut = new HashMap<>();

    final List<String> allNames = new ArrayList<>();
    final Map<String, List<String>> availablePorts = new HashMap<>();


    public BusStatusUIPort(BlockPos boundPos) {
        super(boundPos, BusBlockEntity.STATUS, BusLinkPort.Status.class, BusLinkPort.Status.EMPTY);
    }

    @Override
    public void initLayout(GridLayout layoutToFill) {
        int line = 0;
        layoutToFill.addChild(available, line, 0, 1, 5);
        line++;
        layoutToFill.addChild(availNames, line, 0);
        layoutToFill.addChild(availNameView, line, 1);
        layoutToFill.addChild(availPorts, line, 2);
        layoutToFill.addChild(availPortView, line, 3);
        line++;
        layoutToFill.addChild(addLabel, line, 0, 1, 5);
        layoutToFill.addChild(add, line, 2);
        line++;
        layoutToFill.addChild(exposed, line, 0, 1, 5);
        line++;
        layoutToFill.addChild(usedNamePort, line, 0);
        layoutToFill.addChild(usedNamePortView, line, 1);
        line++;
        layoutToFill.addChild(retLabel, line, 0, 1, 5);
        layoutToFill.addChild(ret, line, 2);
        layoutToFill.columnSpacing(2).rowSpacing(4);
    }

    @Override
    public BusLinkPort.Status readGUI() {
        return new BusLinkPort.Status(Map.of(), Map.of(), currentIn, currentOut);
    }

    private void clear(){
        availableIn.clear();
        availableOut.clear();
        currentIn.clear();
        currentOut.clear();
        allNames.clear();
        availablePorts.clear();
    }

    private static List<String> combine(Set<String> s0, Set<String> s1){
        Set<String> res = new HashSet<>();
        res.addAll(s0);
        res.addAll(s1);
        return res.stream().sorted().toList();
    }

    private void updateWidget(){
        availNameView.setTextOnly(Component.literal("-").withStyle(Converter::optionStyle));
        availPortView.setTextOnly(Component.literal("-").withStyle(Converter::optionStyle));
        usedNamePortView.setTextOnly(Component.literal("----").withStyle(Converter::optionStyle));
        if(allNames.isEmpty())return;
        availNames
            .forOptions(allNames.stream().map(Component::literal).toList())
            .withRange(0, allNames.size())
            .calling(i -> {
                if(i >= allNames.size())return;
                String name = allNames.get(i);
                availNameView.setTextOnly(Component.literal(name).withStyle(Converter::optionStyle));
                List<String> avail = availablePorts.getOrDefault(name, List.of());
                if(avail.isEmpty())return;
                availPorts
                    .forOptions(avail.stream().map(Component::literal).toList())
                    .withRange(0, avail.size())
                    .calling(id -> {
                        if(id >= avail.size())return;
                        String portName = avail.get(id);
                        availPortView.setTextOnly(Component.literal(portName).withStyle(Converter::optionStyle));
                    })
                    .setState(0)
                    .onChanged();
            })
            .setState(0)
            .onChanged();

        List<String> ports = availablePorts.values().stream().flatMap(List::stream).distinct().sorted().toList();
        int maxLenName = MinecraftUtils.maxTitleLength(allNames);
        int maxLenPort = MinecraftUtils.maxTitleLength(ports);
        availNameView.withWidth(maxLenName);
        availPortView.withWidth(maxLenPort);
//
//        usedNamePortView.withWidth(maxLenPort + maxLenPort + 1); // "name:port"

        updateUsed();
        redoLayout();
    }

    private List<String> allUsedName(){
        return ArrayUtils.flatten(currentIn, currentOut);
    }

    private void updateUsed(){
        List<String> allUsed = allUsedName();
        usedNamePortView.setTextOnly(Component.literal("----").withStyle(Converter::optionStyle));
        usedNamePort
                .forOptions(allUsed.stream().map(Component::literal).toList())
                .withRange(0, allUsed.size())
                .calling(i -> {
                    if(i >= allUsed.size()){
                        usedNamePortView.setTextOnly(Component.literal("----").withStyle(Converter::optionStyle));
                        return;
                    }
                    String name = allUsed.get(i);
                    usedNamePortView.setTextOnly(Component.literal(name).withStyle(Converter::optionStyle));
                })
                .setState(0)
                .onChanged();
    }

    private void removeUsed(){
        List<String> allUsed = allUsedName();
        if(usedNamePort.getState() >= allUsed.size()){
            updateUsed();
            return;
        }
        String name = allUsed.get(usedNamePort.getState());
        currentIn.remove(name);
        currentOut.remove(name);

        updateUsed();
    }

    private void addToUsed(){
        if(availNames.getState() >= allNames.size())return;
        String name = allNames.get(availNames.getState());
        List<String> ports = availablePorts.getOrDefault(name, List.of());
        if (ports.isEmpty()) return;
        if (availPorts.getState() >= ports.size())return;
        String port = ports.get(availPorts.getState());
        String compressed = BusPort.compress(name, port);
        if (availableIn.getOrDefault(name, Set.of()).contains(port) && !currentIn.contains(compressed)){
            currentIn.add(compressed);
        }
        if (availableOut.getOrDefault(name, Set.of()).contains(port) && !currentOut.contains(compressed)){
            currentOut.add(compressed);
        }

        updateUsed();
    }

    @Override
    public void writeGUI(BusLinkPort.Status value) {
        clear();
        availableIn.putAll(value.availableIn());
        availableOut.putAll(value.availableOut());
        currentOut.addAll(value.definedOutputs());
        currentIn.addAll(value.definedInputs());
        allNames.addAll(combine(availableIn.keySet(), availableOut.keySet()));
        allNames.forEach(name -> {
            availablePorts.put(name, combine(
                    availableIn.getOrDefault(name, Set.of()),
                    availableOut.getOrDefault(name, Set.of())
            ));
        });


        updateWidget();
    }
}

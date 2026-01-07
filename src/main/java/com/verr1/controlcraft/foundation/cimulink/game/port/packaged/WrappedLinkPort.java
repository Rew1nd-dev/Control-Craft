package com.verr1.controlcraft.foundation.cimulink.game.port.packaged;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.PlantProxy;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.data.links.IntegrationPortStatus;
import kotlin.Pair;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class WrappedLinkPort<W extends NamedComponent> extends BlockLinkPort implements ICompilable<W> {

    W cached;
    List<String> cachedEnabledInputs = new ArrayList<>();
    List<String> cachedEnabledOutputs = new ArrayList<>();

    protected WrappedLinkPort(W initial) {
        super(PlantProxy.of(initial, List.of(), List.of()));
        cached = initial;
    }


    public PlantProxy proxy(){
        return (PlantProxy) __raw();
    }

    @Override
    public NamedComponent create() {
        return PlantProxy.of(
                cached,
                cachedEnabledInputs.stream().sorted().toList(),
                cachedEnabledOutputs.stream().sorted().toList()
        );
    }

    protected List<String> inputNamesValid(){
        return component().inputsExcludeSignals();
    }

    public List<IntegrationPortStatus> viewInputs(){
        List<String> allInputNames = inputNamesValid();
        List<Double> inputValues = allInputNames.stream().map(n -> component().peekInput(n)).toList();

        Set<String> enabled = enabledInput();

        return allInputNames.stream().map(n -> new IntegrationPortStatus(
                n,
                inputValues.get(allInputNames.indexOf(n)),
                true,
                enabled.contains(n)
        )).toList();
    }

    public List<IntegrationPortStatus> viewOutputs(){
        List<String> allOutputNames = component().outputs();
        List<Double> outputValues = allOutputNames.stream().map(n -> component().peekOutput(n)).toList();

        Set<String> enabled = enabledOutput();

        return allOutputNames.stream().map(n -> new IntegrationPortStatus(
                n,
                outputValues.get(allOutputNames.indexOf(n)),
                false,
                enabled.contains(n)
        )).toList();
    }

    public Pair<List<IntegrationPortStatus>, List<IntegrationPortStatus>> viewStatus(){
        return new Pair<>(
                viewInputs(),
                viewOutputs()
        );
    }

    public Set<String> enabledInput(){
        return new HashSet<>(proxy().inputs());
    }

    public Set<String> enabledOutput(){
        return new HashSet<>(proxy().outputs());
    }

    public void setStatus(Pair<List<IntegrationPortStatus>, List<IntegrationPortStatus>> statues){
        List<IntegrationPortStatus> inputStatus = statues.getFirst();
        List<IntegrationPortStatus> outputStatus = statues.getSecond();

        Set<String> currentEnabledInput = enabledInput();
        Set<String> currentEnabledOutput = enabledOutput();

        Set<String> newEnabledInput = inputStatus.stream()
                .filter(IntegrationPortStatus::enabled)
                .map(IntegrationPortStatus::portName)

                .filter(n -> component().hasInput(n))

                .collect(HashSet::new, HashSet::add, HashSet::addAll);

        Set<String> newEnabledOutput = outputStatus.stream()
                .filter(IntegrationPortStatus::enabled)
                .map(IntegrationPortStatus::portName)

                .filter(n -> component().hasOutput(n))

                .collect(HashSet::new, HashSet::add, HashSet::addAll);

        if(ArrayUtils.isSame(newEnabledInput, currentEnabledInput) &&
                ArrayUtils.isSame(newEnabledOutput, currentEnabledOutput)
        ){
            // no new ports enabled
            setValuesOnly(inputStatus);
        }else {
            cachedEnabledInputs = new ArrayList<>(newEnabledInput);
            cachedEnabledOutputs = new ArrayList<>(newEnabledOutput);
            recreate();
            setValuesOnly(inputStatus);
        }
    }

    public void setToAllOpen(){
        var statusIn = viewInputs();
        var statusOut = viewOutputs();
        var openedIn = statusIn.stream().map(s -> new IntegrationPortStatus(s.portName(), s.value(), s.isInput(), true)).toList();
        var openedOut = statusOut.stream().map(s -> new IntegrationPortStatus(s.portName(), s.value(), s.isInput(), true)).toList();
        setStatus(new Pair<>(openedIn, openedOut));
    }

    public void setValuesOnly(List<IntegrationPortStatus> inputStatus){
        try{
            inputStatus.forEach(cps -> {
                component().input(cps.portName(), cps.value());
            });
            component().onInputChange(
                    inputStatus
                            .stream()
                            .map(IntegrationPortStatus::portName)
                            .toArray(String[]::new)
            );
        }catch (IllegalArgumentException e){
            ControlCraft.LOGGER.error("Failed to set input values for CircuitLinkPort: {}", e.getMessage());
        }
    }

}

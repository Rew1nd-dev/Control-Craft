package com.verr1.controlcraft.foundation.cimulink.game.port.packaged;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.links.circuit.CircuitBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.PlantProxy;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.data.links.CircuitPortStatus;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import kotlin.Pair;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


public class CircuitLinkPort extends BlockLinkPort implements ICompilable<Circuit> {



    private @NotNull CircuitNbt nbt = CircuitNbt.EMPTY_CONTEXT;

    Circuit cachedCircuit = CircuitNbt.EMPTY_CIRCUIT;
    List<String> cachedEnabledInputs = new ArrayList<>();
    List<String> cachedEnabledOutputs = new ArrayList<>();

    public CircuitLinkPort() {
        super(PlantProxy.of(CircuitNbt.EMPTY_CIRCUIT, List.of(), List.of()));
    }


    public void load(@Nullable CircuitNbt nbt){
        this.nbt = nbt == null ? CircuitNbt.EMPTY_CONTEXT : nbt;
        buildCached();
        recreate();
    }

    private void buildCached(){
        cachedCircuit = nbt.buildCircuit();
        cachedEnabledInputs = cachedCircuit.inputNamesValid();
        cachedEnabledOutputs = cachedCircuit.outputs();
    }

    @Override
    public NamedComponent create() {
        return PlantProxy.of(
                cachedCircuit,
                cachedEnabledInputs.stream().sorted().toList(),
                cachedEnabledOutputs.stream().sorted().toList()
        );
    }

    public Circuit circuit(){
        return (Circuit)(proxy().plant());
    }

    public PlantProxy proxy(){
        return (PlantProxy) __raw();
    }

    @Override
    public Circuit component() {
        return circuit();
    }

    public List<CircuitPortStatus> viewInputs(){
        List<String> allInputNames = circuit().inputNamesValid();
        List<Double> inputValues = allInputNames.stream().map(n -> circuit().peekInput(n)).toList();

        Set<String> enabled = enabledInput();

        return allInputNames.stream().map(n -> new CircuitPortStatus(
                n,
                inputValues.get(allInputNames.indexOf(n)),
                true,
                enabled.contains(n)
        )).toList();
    }

    public List<CircuitPortStatus> viewOutputs(){
        List<String> allOutputNames = circuit().outputs();
        List<Double> outputValues = allOutputNames.stream().map(n -> circuit().peekOutput(n)).toList();

        Set<String> enabled = enabledOutput();

        return allOutputNames.stream().map(n -> new CircuitPortStatus(
                n,
                outputValues.get(allOutputNames.indexOf(n)),
                false,
                enabled.contains(n)
        )).toList();
    }

    public Pair<List<CircuitPortStatus>, List<CircuitPortStatus>> viewStatus(){
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

    public void setStatus(Pair<List<CircuitPortStatus>, List<CircuitPortStatus>> statues){
        List<CircuitPortStatus> inputStatus = statues.getFirst();
        List<CircuitPortStatus> outputStatus = statues.getSecond();

        Set<String> currentEnabledInput = enabledInput();
        Set<String> currentEnabledOutput = enabledOutput();

        Set<String> newEnabledInput = inputStatus.stream()
                .filter(CircuitPortStatus::enabled)
                .map(CircuitPortStatus::portName)
                .collect(HashSet::new, HashSet::add, HashSet::addAll);

        Set<String> newEnabledOutput = outputStatus.stream()
                .filter(CircuitPortStatus::enabled)
                .map(CircuitPortStatus::portName)
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

    private void setValuesOnly(List<CircuitPortStatus> inputStatus){
        try{
            inputStatus.forEach(cps -> {
                circuit().input(cps.portName(), cps.value());
            });
            circuit().onInputChange(
                    inputStatus
                            .stream()
                            .map(CircuitPortStatus::portName)
                            .toArray(String[]::new)
            );
        }catch (IllegalArgumentException e){
            ControlCraft.LOGGER.error("Failed to set input values for CircuitLinkPort: {}", e.getMessage());
        }
    }


    @Override
    public CimulinkFactory.Factory<Circuit> factory() {
        return CimulinkFactory.CIRCUIT;
    }


    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("circuit", nbt.serialize())
                .withCompound("status", CircuitBlockEntity.PAIR_SER.serialize(viewStatus()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        load(CircuitNbt.deserialize(tag.getCompound("circuit")));
        if(tag.contains("status"))setStatus(CircuitBlockEntity.PAIR_SER.deserialize(tag.getCompound("status")));

        super.deserialize(tag.getCompound("blp"));
    }
}

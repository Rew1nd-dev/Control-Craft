package com.verr1.controlcraft.foundation.cimulink.game.port.plant;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.Sink;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.PlantProxy;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.links.PortStatus;
import com.verr1.controlcraft.foundation.data.links.ProxyPortStatus;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class PlantProxyLinkPort extends BlockLinkPort {

    private final NamedComponent EMPTY = new Sink();

    NamedComponent plant = EMPTY;

    Set<Integer> enabledInput = new HashSet<>();
    Set<Integer> enabledOutput = new HashSet<>();

    public PlantProxyLinkPort() {
        super(new Sink());
    }

    public void setPlant(@Nullable NamedComponent plant){
        this.plant = plant == null ? EMPTY : plant;
        enabledInput.clear();
        enabledOutput.clear();
        recreate();
    }

    public void setEnabledInput(int index, boolean enable){
        if(index < 0 || index >= plant.n())return;
        if(enable)enabledInput.add(index);
        else enabledInput.remove(index);
        recreate();
    }

    public void setEnabledInput(String name, boolean enable){
        if(!plant.namedInputs().containsKey(name))return;
        setEnabledInput(plant.in(name), enable);
    }

    public void setEnabledOutput(int index, boolean enable){
        if(index < 0 || index >= plant.m())return;
        if(enable)enabledOutput.add(index);
        else enabledOutput.remove(index);
        recreate();
    }

    public void setEnabledOutput(String name, boolean enable){
        if(!plant.namedOutputs().containsKey(name))return;
        setEnabledOutput(plant.out(name), enable);
    }

    public ProxyPortStatus viewInput(){
        List<PortStatus> ps = plant.inputs()
                .stream()
                .map(inName -> new PortStatus(inName, enabledInput.contains(plant.in(inName))))
                .toList();
        return new ProxyPortStatus(ps);
    }

    private void safeAddInput(String name){
        if(!plant.namedInputs().containsKey(name))return;
        enabledInput.add(plant.in(name));
    }

    private void safeAddOutput(String name){
        if(!plant.namedOutputs().containsKey(name))return;
        enabledOutput.add(plant.out(name));
    }

    public void setInput(ProxyPortStatus status){
        Set<Integer> copy = Set.copyOf(enabledInput);
        enabledInput.clear();
        status.statuses().stream()
                .filter(PortStatus::enabled)
                .map(PortStatus::name)
                .forEach(this::safeAddInput);

        if(copy.size() == enabledInput.size() && enabledInput.containsAll(copy)){
            // Nothing changed
            return;
        }
        recreate();
    }

    public void setOutput(ProxyPortStatus status){
        Set<Integer> copy = Set.copyOf(enabledOutput);
        enabledOutput.clear();
        status.statuses().stream()
                .filter(PortStatus::enabled)
                .map(PortStatus::name)
                .forEach(this::safeAddOutput);

        if(copy.size() == enabledOutput.size() && enabledOutput.containsAll(copy)){
            // Nothing changed
            return;
        }
        recreate();
    }


    public ProxyPortStatus viewOutput(){
        List<PortStatus> ps = plant.outputs()
                .stream()
                .map(outName -> new PortStatus(outName, enabledOutput.contains(plant.out(outName))))
                .toList();
        return new ProxyPortStatus(ps);
    }

    public ProxyPortStatus viewAll(){
        return new ProxyPortStatus(ArrayUtils.flatten(viewInput().statuses(), viewOutput().statuses()));
    }

    public void setAll(ProxyPortStatus status){
        Set<Integer> outCopy = Set.copyOf(enabledOutput);
        Set<Integer> inCopy = Set.copyOf(enabledInput);
        enabledInput.clear();
        enabledOutput.clear();
        status.statuses().stream()
                .filter(PortStatus::enabled)
                .map(PortStatus::name)
                .forEach(n -> {safeAddOutput(n);safeAddInput(n);});
        // only one will add, because inputNames and outputNames should be different by definition,
        // see NamedComponent::new
        if(         outCopy.size() == enabledOutput.size() && enabledOutput.containsAll(outCopy)
                &&  inCopy.size() == enabledInput.size() && enabledInput.containsAll(inCopy)
        ){
            // Nothing changed
            return;
        }

        recreate();
    }

    @Override
    public NamedComponent create() {
        return new PlantProxy(
                plant,
                enabledInput.stream().sorted().toList(),
                enabledOutput.stream().sorted().toList()
        );
    }



}

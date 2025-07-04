package com.verr1.controlcraft.foundation.cimulink.game.port.plant;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.links.proxy.ProxyLinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.Sink;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.PlantProxy;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.links.StringBoolean;
import com.verr1.controlcraft.foundation.data.links.StringBooleans;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

public class PlantProxyLinkPort extends BlockLinkPort {

    public static final SerializeUtils.Serializer<StringBooleans> PROXY_PORT =
            SerializeUtils.of(
                    StringBooleans::serialize,
                    StringBooleans::deserialize
            );

    private final NamedComponent EMPTY = new Sink();

    NamedComponent plant = EMPTY;

    Set<Integer> enabledInput = new HashSet<>();
    Set<Integer> enabledOutput = new HashSet<>();

    private final ProxyLinkBlockEntity be;

    public PlantProxyLinkPort(@NotNull ProxyLinkBlockEntity be) {
        super(new Sink());
        this.be = be;
    }

    public void setPlant(@Nullable NamedComponent plant){
        if(this.plant == plant)return;
        ControlCraft.LOGGER.debug("Setting plant in PlantProxyLinkPort: {} at: {}", plant, be.getBlockPos());

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

    @Override
    public boolean isCombinational() {
        return false;
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

    public StringBooleans viewInput(){
        List<StringBoolean> ps = plant.inputs()
                .stream()
                .map(inName -> new StringBoolean(inName, enabledInput.contains(plant.in(inName))))
                .toList();
        return new StringBooleans(ps);
    }

    private void safeAddInput(String name){
        if(!plant.namedInputs().containsKey(name))return;
        enabledInput.add(plant.in(name));
    }

    private void safeAddOutput(String name){
        if(!plant.namedOutputs().containsKey(name))return;
        enabledOutput.add(plant.out(name));
    }

    public void setInput(StringBooleans status){
        Set<Integer> copy = Set.copyOf(enabledInput);
        enabledInput.clear();
        status.statuses().stream()
                .filter(StringBoolean::enabled)
                .map(StringBoolean::name)
                .forEach(this::safeAddInput);

        if(copy.size() == enabledInput.size() && enabledInput.containsAll(copy)){
            // Nothing changed
            return;
        }
        recreate();
    }

    public void setOutput(StringBooleans status){
        Set<Integer> copy = Set.copyOf(enabledOutput);
        enabledOutput.clear();
        status.statuses().stream()
                .filter(StringBoolean::enabled)
                .map(StringBoolean::name)
                .forEach(this::safeAddOutput);

        if(copy.size() == enabledOutput.size() && enabledOutput.containsAll(copy)){
            // Nothing changed
            return;
        }
        recreate();
    }


    public StringBooleans viewOutput(){
        List<StringBoolean> ps = plant.outputs()
                .stream()
                .map(outName -> new StringBoolean(outName, enabledOutput.contains(plant.out(outName))))
                .toList();
        return new StringBooleans(ps);
    }

    public StringBooleans viewAll(){
        return new StringBooleans(ArrayUtils.flatten(viewInput().statuses(), viewOutput().statuses()));
    }

    public void setAll(StringBooleans status){
        Set<Integer> outCopy = Set.copyOf(enabledOutput);
        Set<Integer> inCopy = Set.copyOf(enabledInput);
        enabledInput.clear();
        enabledOutput.clear();
        status.statuses().stream()
                .filter(StringBoolean::enabled)
                .map(StringBoolean::name)
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

    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("status", PROXY_PORT.serialize(viewAll()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        be.updateAttachedPlant(); // set plant
        ControlCraft.LOGGER.debug("Deserializing PlantProxyLinkPort");
        setAll(PROXY_PORT.deserialize(tag.getCompound("status"))); // set status
        ControlCraft.LOGGER.debug("Deserializing status");
        super.deserialize(tag.getCompound("blp")); // restore links
        ControlCraft.LOGGER.debug("Deserializing links");
    }
}

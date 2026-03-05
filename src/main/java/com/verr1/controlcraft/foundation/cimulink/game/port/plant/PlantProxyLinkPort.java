package com.verr1.controlcraft.foundation.cimulink.game.port.plant;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.Sink;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.PlantProxy;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.links.StringBoolean;
import com.verr1.controlcraft.foundation.data.links.StringBooleans;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.Nullable;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class PlantProxyLinkPort extends BlockLinkPort {

    public static final Serializer<StringBooleans> PROXY_PORT =
            SerializeUtils.of(
                    StringBooleans::serialize,
                    StringBooleans::deserialize
            );

    public static final Serializer<Set<Integer>> INT_SET_SER = SerializeUtils.ofSet(SerializeUtils.INT);

    private static final NamedComponent EMPTY = new Sink();

    NamedComponent plant = EMPTY;
    Set<Integer> enabledInput = new HashSet<>();
    Set<Integer> enabledOutput = new HashSet<>();

    // private final ProxyLinkBlockEntity be;

    public PlantProxyLinkPort() { //@NotNull ProxyLinkBlockEntity be
        super(new Sink());
        // this.be = be;
    }

    public void setPlant(@Nullable NamedComponent plant){
        if(this.plant == plant)return;
        // ControlCraft.LOGGER.debug("Setting plant in PlantProxyLinkPort: {} at: {}", plant, be.getBlockPos());

        this.plant = plant == null ? EMPTY : plant;
        enabledInput.clear();
        enabledOutput.clear();

        int n = this.plant.n();
        for(int i = 0; i < Math.min(n, 8); i++){
            enabledInput.add(i);
        }
        int m = this.plant.m();
        for(int i = 0; i < Math.min(m, 8); i++){
            enabledOutput.add(i);
        }
    }

//    public void setEnabledInput(int index, boolean enable){
//        if(index < 0 || index >= plant.n())return;
//        if(enable)enabledInput.add(index);
//        else enabledInput.remove(index);
//    }

    @Override
    public boolean isCombinational() {
        return false;
    }

//    public void setEnabledInput(String name, boolean enable){
//        if(!plant.namedInputs().containsKey(name))return;
//        setEnabledInput(plant.in(name), enable);
//    }

//    public void setEnabledOutput(int index, boolean enable){
//        if(index < 0 || index >= plant.m())return;
//        if(enable)enabledOutput.add(index);
//        else enabledOutput.remove(index);
//    }

//    public void setEnabledOutput(String name, boolean enable){
//        if(!plant.namedOutputs().containsKey(name))return;
//        setEnabledOutput(plant.out(name), enable);
//    }

    public StringBooleans viewInput(){
        List<StringBoolean> ps = plant.inputs()
                .stream()
                .map(inName -> new StringBoolean(inName, enabledInput.contains(plant.in(inName))))
                .toList();
        return new StringBooleans(ps);
    }

    //    public void setInput(StringBooleans status){
//        Set<Integer> copy = Set.copyOf(enabledInput);
//        enabledInput.clear();
//        status.statuses().stream()
//                .filter(StringBoolean::enabled)
//                .map(StringBoolean::name)
//                .forEach(this::safeAddInput);
//
//        if(copy.size() == enabledInput.size() && enabledInput.containsAll(copy)){
//            // Nothing changed
//            return;
//        }
//    }

//    public void setOutput(StringBooleans status){
//        Set<Integer> copy = Set.copyOf(enabledOutput);
//        enabledOutput.clear();
//        status.statuses().stream()
//                .filter(StringBoolean::enabled)
//                .map(StringBoolean::name)
//                .forEach(this::safeAddOutput);
//
//        if(copy.size() == enabledOutput.size() && enabledOutput.containsAll(copy)){
//            // Nothing changed
//            return;
//        }
//    }


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

    public void setAllAndUpdate(StringBooleans status){
        Set<Integer> outCopy = Set.copyOf(enabledOutput);
        Set<Integer> inCopy = Set.copyOf(enabledInput);
        enabledInput.clear();
        enabledOutput.clear();
        status.statuses().stream()
            .filter(StringBoolean::enabled)
            .map(StringBoolean::name)
            .forEach(n -> {
                if(plant.namedOutputs().containsKey(n)) {
                    enabledOutput.add(plant.out(n));
                }
                if(plant.namedInputs().containsKey(n)){
                    enabledInput.add(plant.in(n));
                }

            });
        // only one will add, because inputNames and outputNames should be different by definition,
        // see NamedComponent::new
        if(     outCopy.size() == enabledOutput.size() && enabledOutput.containsAll(outCopy)
            &&  inCopy.size() == enabledInput.size() && enabledInput.containsAll(inCopy)
        ){
            // Nothing changed
            return;
        }

        recreate();
    }

    public boolean isEnableSettingsValid(){
        return enabledInput.stream().allMatch(i -> i >= 0 && i < plant.n())
            && enabledOutput.stream().allMatch(i -> i >= 0 && i < plant.m());
    }

    @Override
    public NamedComponent create() {
        if(!isEnableSettingsValid()){
            enabledInput.clear();
            enabledOutput.clear();
            return __raw();
        }
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
                .withCompound("enabledInput", INT_SET_SER.serialize(enabledInput))
                .withCompound("enabledOutput", INT_SET_SER.serialize(enabledOutput))//PROXY_PORT.serialize(viewAll())
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        if(tag.contains("enabledInput")){
            enabledInput.clear();
            enabledInput.addAll(INT_SET_SER.deserialize(tag.getCompound("enabledInput")));
        }
        if(tag.contains("enabledOutput")){
            enabledInput.clear();
            enabledInput.addAll(INT_SET_SER.deserialize(tag.getCompound("enabledOutput")));
        }
        super.deserialize(tag.getCompound("blp"));
    }
}

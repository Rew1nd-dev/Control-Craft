package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.*;

public class PlantProxy extends NamedComponent {

    NamedComponent plant;
    List<Integer> inputMapping;
    List<Integer> outputMapping;
    List<Integer> reverseOutputMapping;


    public PlantProxy(
            NamedComponent plant,
            List<Integer> enabledInput,
            List<Integer> enabledOutput
    ) {
        /*
        The constructor below will actually do the following:
        enabledInput.forEach(i -> ArrayUtils.AssertRange(i, plant.n()));
        enabledInput.forEach(i -> ArrayUtils.AssertRange(i, plant.n()));
        * */
        super(
                enabledInput.stream().map(plant::in).toList(),
                enabledOutput.stream().map(plant::out).toList()
        );
        this.plant = plant;
        this.inputMapping = List.copyOf(enabledInput);
        this.outputMapping = List.copyOf(enabledOutput);
        reverseOutputMapping = new ArrayList<>(ArrayUtils.ListOf(plant.m(), -1));
        for (int i = 0; i < outputMapping.size(); i++){
            reverseOutputMapping.set(outputMapping.get(i), i);
        }
    }

    @Override
    public List<Integer> propagateTo(int proxyIn) {
        return plant.propagateTo(mapIn(proxyIn)).stream().map(this::mapOut).toList();
    }

    private int mapIn(int proxyIn){
        ArrayUtils.AssertRange(proxyIn, n());
        return inputMapping.get(proxyIn);
    }

    private int mapOut(int plantOut){
        ArrayUtils.AssertRange(plantOut, plant.m());
        int res = reverseOutputMapping.get(plantOut);
        if(res == -1){
            throw new IllegalStateException("mapping is incorrect: r:" + reverseOutputMapping + "--" + outputMapping + " in: " + plantOut);
        }
        return res;
    }



    @Override
    public void onInputChange(Integer... inputIndexes) {
        Arrays.stream(inputIndexes).forEach(
                i -> plant.input(mapIn(i), retrieveInput(i))
        );
        plant.onInputChange(
                Arrays.stream(inputIndexes).map(this::mapIn).toArray(Integer[]::new)
        );
        updateOutput();
    }

    private void updateOutput(){
        plant.changedOutput().stream().filter(i -> reverseOutputMapping.get(i) != -1).forEach(
                co -> updateOutput(mapOut(co), plant.retrieveOutput(co))
        );
    }

    @Override
    public void onPositiveEdge() {
        plant.onPositiveEdge();
        updateOutput();
    }
}

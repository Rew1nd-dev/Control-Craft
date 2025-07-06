package com.verr1.controlcraft.foundation.cimulink.game.port.digital;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.LinearAdder;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.ICompilable;
import com.verr1.controlcraft.foundation.cimulink.game.registry.CimulinkFactory;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.nbt.CompoundTag;

import java.util.ArrayList;
import java.util.List;

public class FMALinkPort extends BlockLinkPort implements ICompilable<LinearAdder> {

    private final List<Double> cachedNewCoefficients = new ArrayList<>(List.of(1.0, 1.0));
    public static final Serializer<List<Double>> COEFF_SERIALIZER =
            SerializeUtils.ofList(SerializeUtils.DOUBLE);

    public FMALinkPort() {
        super(new LinearAdder(List.of(1.0, 1.0)));
    }

    public void setCoefficients(List<Double> all){
        if(all.size() != n()){ // n() is equal to coeffs.size(), normally
            resetCoefficients(all);
        }else {
            ((LinearAdder)__raw()).setCoefficients(all);
        }
    }

    public void setCoefficient(int index, double coeff){
        if(index < 0 || index > n())return;
        ((LinearAdder)__raw()).setCoefficient(index, coeff);
    }

    public List<Double> viewCoefficients(){
        return ((LinearAdder)__raw()).viewCoefficients();
    }

    private void setCached(List<Double> all){
        cachedNewCoefficients.clear();
        cachedNewCoefficients.addAll(all);
    }



    private void resetCoefficients(List<Double> all){
        setCached(all);
        recreate();
    }


    public void setNamedCoefficients(List<Pair<String, Double>> all){
        if(all.size() != n()){ // n() is equal to coeffs.size(), normally
            // resetNamedCoefficients(all);
            return;
        }else {
            ((LinearAdder)__raw()).setNamedCoefficients(all);
        }
    }

    public void setNamedCoefficient(String index, double coeff){
        ((LinearAdder)__raw()).setNamedCoefficient(index, coeff);
    }

    public List<Pair<String, Double>> viewNamedCoefficients(){
        return ((LinearAdder)__raw()).viewNamedCoefficients();
    }


    private void resetNamedCoefficients(List<Pair<String, Double>> all){
        List<Double> arranged = new ArrayList<>(ArrayUtils.ListOf(all.size(), 1.0));
        for (Pair<String, Double> stringDoublePair : all) {
            arranged.set(in(stringDoublePair.getFirst()), stringDoublePair.getSecond());
        }
        resetCoefficients(arranged);
    }

    @Override
    public NamedComponent create() {
        return new LinearAdder(cachedNewCoefficients);
    }


    @Override
    public CompoundTag serialize() {
        return CompoundTagBuilder.create()
                .withCompound("blp", super.serialize())
                .withCompound("coeff", COEFF_SERIALIZER.serialize(viewCoefficients()))
                .build();
    }

    @Override
    public void deserialize(CompoundTag tag) {
        if(tag.contains("coeff")){
            resetCoefficients(
                    COEFF_SERIALIZER.deserialize(tag.getCompound("coeff"))
            );
        }
        super.deserialize(tag.getCompound("blp"));
    }


    @Override
    public LinearAdder component() {
        return (LinearAdder)__raw();
    }

    @Override
    public CimulinkFactory.Factory<LinearAdder> factory() {
        return CimulinkFactory.FMA;
    }
}

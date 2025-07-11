package com.verr1.controlcraft.foundation.cimulink.core.components.analog;



import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.registry.CimulinkFactory;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.nbt.CompoundTag;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

public class LinearAdder extends Combinational {
    private static final Serializer<List<Double>> COEFF =
            SerializeUtils.ofList(SerializeUtils.DOUBLE);

    private final ArrayList<Double> coefficients;
    private final List<Double> coefficientsView;

    public LinearAdder(List<Double> coefficients) {
        super(
                ArrayUtils.createInputNames(coefficients.size()),
                ArrayUtils.SINGLE_OUTPUT
        );
        this.coefficients = new ArrayList<>(coefficients);
        coefficientsView = Collections.unmodifiableList(this.coefficients);
    }

    public void setCoefficients(List<Double> newCoefficients){
        if(newCoefficients.size() != coefficients.size())return;
        for (int i = 0; i < coefficients.size(); i++)coefficients.set(i, newCoefficients.get(i));
    }

    public void setCoefficient(int index, double value){
        if(index < 0 || index >= n())return;
        coefficients.set(index, value);
    }

    public List<Double> viewCoefficients(){
        return coefficientsView;
    }

    public void setNamedCoefficients(List<Pair<String, Double>> newCoefficients){
        if(newCoefficients.size() != coefficients.size())return;
        for (Pair<String, Double> newCoefficient : newCoefficients)
            setNamedCoefficient(newCoefficient.getFirst(), newCoefficient.getSecond());
    }

    public void setNamedCoefficient(String name, double value){
        setCoefficient(in(name), value);
    }

    public List<Pair<String, Double>> viewNamedCoefficients(){
        return IntStream.range(0, n()).mapToObj(i -> new Pair<>(in(i), coefficients.get(i))).toList();
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withCompound("coeff", COEFF.serialize(coefficients))
                .build();
    }

    public static LinearAdder deserialize(CompoundTag tag){
        return new LinearAdder(
                COEFF.deserialize(tag.getCompound("coeff"))
        );
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        ArrayUtils.AssertSize(inputs, n());
        double result = 0;
        for (int i = 0; i < n(); i++) {
            result += inputs.get(i) * coefficients.get(i);
        }
        return List.of(result);
    }

    @Override
    public Factory<? extends NamedComponent> factory() {
        return CimulinkFactory.FMA;
    }
}

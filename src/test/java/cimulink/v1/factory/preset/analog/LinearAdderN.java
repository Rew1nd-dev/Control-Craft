package cimulink.v1.factory.preset.analog;

import cimulink.v1.factory.basic.analog.AnalogNM;
import cimulink.v1.utils.ArrayUtils;

import java.util.ArrayList;
import java.util.List;

public class LinearAdderN extends AnalogNM<Void> {

    public LinearAdderN(List<Double> coefficients) {
        super(
                ArrayUtils.createInputNames(coefficients.size()),
                List.of(ArrayUtils.OUTPUT_O),
                in -> fma(new ArrayList<>(coefficients), in) // immutable
        );
    }

    public String in(int index){
        if(index < 0 || index >= inputs().size()){
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for inputs.");
        }
        return ArrayUtils.__in(index);
    }

    public String out(){
        return ArrayUtils.__o();
    }

    private static List<Double> fma(List<Double> coefficients, List<Double> inputs){
        if(coefficients.size() != inputs.size()){
            throw new IllegalArgumentException("Coefficients and inputs must have the same size.");
        }
        double result = 0;
        for (int i = 0; i < coefficients.size(); i++) {
            result += coefficients.get(i) * inputs.get(i);
        }
        return List.of(result);
    }


}

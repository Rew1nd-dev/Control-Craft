package cimulink.factory.preset;

import cimulink.factory.basic.ComponentNM;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

public class LinearAdderN extends ComponentNM<Void> {
    public static final String OUTPUT_O = "o";
    public static final String INPUT_PREFIX = "i_";

    public LinearAdderN(List<Double> coefficients) {
        super(
                createInputNames(coefficients.size()),
                List.of(OUTPUT_O),
                in -> fma(new ArrayList<>(coefficients), in) // immutable
        );
    }

    public static String __in(int index){
        return INPUT_PREFIX + index;
    }

    public String in(int index){
        if(index < 0 || index >= inputs().size()){
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for inputs.");
        }
        return __in(index);
    }

    public String out(){
        return __o();
    }

    public static String __o() {
        return OUTPUT_O;
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


    public static List<String> createInputNames(int n){
        return IntStream.range(0, n).mapToObj(LinearAdderN::__in).toList();
    }


}

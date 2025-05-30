package cimulink.v1.factory.preset.digital.gates;

import cimulink.v1.factory.basic.digital.DigitalNM;
import cimulink.v1.utils.ArrayUtils;

import java.util.List;

public class AndN extends DigitalNM<Void> {

    public AndN(int N) {
        super(
                ArrayUtils.createInputNames(N),
                ArrayUtils.SINGLE_OUTPUT,
                AndN::and
        );
    }

    private static List<Boolean> and(List<Boolean> in){
        return List.of(in.stream().reduce(true, (a, b) -> a && b));
    }
}

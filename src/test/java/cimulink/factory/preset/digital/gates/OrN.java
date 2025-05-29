package cimulink.factory.preset.digital.gates;

import cimulink.factory.basic.digital.DigitalNM;
import cimulink.utils.ArrayUtils;

import java.util.List;

public class OrN extends DigitalNM<Void> {

    public OrN(int N) {
        super(
                ArrayUtils.createInputNames(N),
                ArrayUtils.SINGLE_OUTPUT,
                OrN::or
        );
    }

    private static List<Boolean> or(List<Boolean> in){
        return List.of(in.stream().reduce(false, (a, b) -> a || b));
    }
}

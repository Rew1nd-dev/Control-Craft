package cimulink.v1.factory.preset.digital.gates;

import cimulink.v1.factory.basic.digital.DigitalNM;
import cimulink.v1.utils.ArrayUtils;

import java.util.List;

public class XorN extends DigitalNM<Void> {

    public XorN(int N) {
        super(
                ArrayUtils.createInputNames(N),
                ArrayUtils.SINGLE_OUTPUT,
                XorN::xor
        );
    }

    private static List<Boolean> xor(List<Boolean> in){
        return List.of(in.stream().reduce(false, (a, b) -> a ^ b));
    }
}

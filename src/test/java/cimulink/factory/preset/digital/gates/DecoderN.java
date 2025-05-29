package cimulink.factory.preset.digital.gates;

import cimulink.factory.basic.digital.DigitalNM;
import cimulink.utils.ArrayUtils;

import java.util.Arrays;
import java.util.List;

public class DecoderN extends DigitalNM<Void> {

    public DecoderN(int N) {
        super(
                ArrayUtils.createInputNames(N),
                ArrayUtils.createInputNames(1 << N),
                DecoderN::decode
        );
    }

    private static List<Boolean> decode(List<Boolean> in){
        if (in.isEmpty()) {
            throw new IllegalArgumentException("Input list cannot be empty.");
        }
        int index = 0;
        for (int i = 0; i < in.size(); i++) {
            if (in.get(i)) {
                index |= (1 << i);
            }
        }
        List<Boolean> output = Arrays.asList(new Boolean[1 << in.size()]);
        output.set(index, true);
        return output;
    }
}

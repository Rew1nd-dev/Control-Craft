package cimulink.v1.factory.preset.digital.gates;

import cimulink.v1.factory.basic.digital.DigitalNM;
import cimulink.v1.utils.ArrayUtils;

import java.util.Arrays;
import java.util.List;

public class EncoderN extends DigitalNM<Void> {



    public EncoderN(int N) {
        super(
                ArrayUtils.createInputNames(1 << N),
                ArrayUtils.createInputNames(N),
                arr -> encode(arr, N)
        );

    }

    private static List<Boolean> encode(List<Boolean> in, int n){
        if (in.isEmpty()) {
            throw new IllegalArgumentException("Input list cannot be empty.");
        }
        if(in.size() != 1 << n){
            throw new IllegalArgumentException("Input list size must be 2^N for N = " + n);
        }


        int first = 0;
        for (int i = 0; i < in.size(); i++) {
            if (!in.get(i))continue;
            first = i;
            break;
        }

        List<Boolean> output = Arrays.asList(new Boolean[n]);

        for (int i = 0; i < n; i++) {
            output.set(i, (first & (1 << i)) != 0);
        }

        return output;
    }

}

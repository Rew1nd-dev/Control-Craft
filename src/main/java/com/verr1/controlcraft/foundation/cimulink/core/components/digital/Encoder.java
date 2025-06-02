package com.verr1.controlcraft.foundation.cimulink.core.components.digital;



import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.List;

public class Encoder extends BooleanCombinational {
    public Encoder(int n) {
        super(
                ArrayUtils.createInputNames(1 << n),
                ArrayUtils.createOutputNames(n)
        );
    }

    @Override
    protected List<Boolean> transformBoolean(List<Boolean> inputs) {
        int index = 0;
        for (int i = 0; i < inputs.size(); i++) {
            if (inputs.get(i)) {
                index = i;
                break;
            }
        }
        return encode(index, m());

    }

    public static List<Boolean> encode(int input, int size) {
        List<Boolean> outputs = ArrayUtils.ListOf(size, false);
        for (int i = 0; i < size; i++) {
            outputs.set(i, (input & (1 << i)) != 0);
        }
        return outputs;
    }

}

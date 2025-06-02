package cimulink.v3.components.digital;

import cimulink.v3.utils.ArrayUtils;

import java.util.List;

public class Decoder extends BooleanCombinational{
    public Decoder(int n) {
        super(
                ArrayUtils.createInputNames(n),
                ArrayUtils.createOutputNames(1 << n)
        );
    }

    @Override
    protected List<Boolean> transformBoolean(List<Boolean> inputs) {
        List<Boolean> outputs = ArrayUtils.ListOf(m(), false);
        outputs.set(decode(inputs), true);
        return outputs;

    }


    public static int decode(List<Boolean> inputs) {
        int index = 0;
        for (int i = 0; i < inputs.size(); i++) {
            if (inputs.get(i)) {
                index |= (1 << i);
            }
        }
        return index;

    }
}

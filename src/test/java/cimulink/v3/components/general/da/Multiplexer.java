package cimulink.v3.components.general.da;

import cimulink.v3.components.digital.Decoder;
import cimulink.v3.components.general.Combinational;
import cimulink.v3.records.ComponentPortName;
import cimulink.v3.utils.ArrayUtils;

import java.util.List;

public class Multiplexer extends Combinational {

    private final int bits;

    @SuppressWarnings("unchecked")
    public Multiplexer(int n) {
        super(
                (List<String>)ArrayUtils.flatten(
                        ArrayUtils.createWithPrefix(
                                "sel_",
                                n
                        ),
                        ArrayUtils.createInputNames(1 << n)
                ),
                ArrayUtils.SINGLE_OUTPUT
        );
        bits = n;
    }

    public String dat(int i){
        return in(i + bits);
    }

    public String sel(int i){
        return in(i);
    }

    public ComponentPortName __sel(int i){
        return __in(i);
    }

    public ComponentPortName __dat(int i){
        return __in(i + bits);
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        List<Boolean> sel = ArrayUtils.mapToList(
                inputs.subList(0, bits),
                d -> d > 0.5
        );

        List<Double> dat = inputs.subList(bits, inputs.size());

        return List.of(transformInternal(sel, dat));
    }

    protected Double transformInternal(List<Boolean> sel, List<Double> dat) {
        return dat.get(Decoder.decode(sel));

    }

}

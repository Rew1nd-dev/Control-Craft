package cimulink.v2.components.compiled;

import cimulink.v2.components.NamedComponent;

import java.util.List;

public abstract class Combinational extends NamedComponent {

    public Combinational(List<String> inputs, List<String> outputs) {
        super(inputs, outputs);
    }

    @Override
    public final void onInputChange() {
        updateOutput(transform(retrieveInput()));
    }

    @Override
    public final void onPositiveEdge() {}

    @Override
    protected final boolean immediateInternal(int $) {
        return true;
    }

    protected abstract List<Double> transform(List<Double> inputs);

}

package cimulink.v2.components.compiled;

import cimulink.v2.Component;

import java.util.List;

public abstract class Combinational extends Component {

    public Combinational(int n, int m) {
        super(n, m);
    }

    @Override
    public final void onInputChange() {
        updateOutput(transform(retrieveInput()));
    }

    @Override
    public final void onPositiveEdge() {}

    protected abstract List<Double> transform(List<Double> inputs);

}

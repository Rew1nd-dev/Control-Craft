package cimulink.components;

import cimulink.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class SimpleWireComponent extends Component {

    protected SimpleWireComponent(int inputs, int outputs) {
        super(true, inputs, outputs);
    }

    @Override
    public List<Double> supply() {
        return IntStream.range(0, M()).mapToDouble(i -> i).boxed().toList();
    }

    @Override
    public void consume(List<Double> inputs) {
        if(inputs.size() != N()) {
            throw new IllegalArgumentException("Expected " + N() + " inputs, but got " + inputs.size());
        }
        return;
    }
}

package cimulink.components;

import cimulink.Component;

import java.util.List;
import java.util.stream.IntStream;

public class SimpleRegComponent extends Component {



    public SimpleRegComponent(int inputs, int outputs) {
        super(false, inputs, outputs);

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

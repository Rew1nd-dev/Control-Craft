package cimulink.v1.components;

import cimulink.v1.Circuit;
import cimulink.v1.Component;

import java.util.List;

public class CircuitComponent extends Component {

    private final Circuit circuit;


    CircuitComponent(Circuit circuit, boolean immediate, int N, int M) {
        super(immediate, N, M);
        this.circuit = circuit;
    }


    public CircuitComponent of(Circuit circuit){
        return null;
    }


    @Override
    public List<Double> supply() {
        return List.of();
    }

    @Override
    public void consume(List<Double> inputs) {

    }

    @Override
    public void transit() {

    }
}

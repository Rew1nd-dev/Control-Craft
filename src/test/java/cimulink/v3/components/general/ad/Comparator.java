package cimulink.v3.components.general.ad;

import cimulink.v3.components.general.Combinational;
import cimulink.v3.records.ComponentPortName;

import java.util.List;

public class Comparator extends Combinational {
    public Comparator() {
        super(
                List.of("A", "B"),
                List.of("A>B", "A<B", "A=B")
        );
    }





    @Override
    protected List<Double> transform(List<Double> inputs) {
        double a = inputs.get(0);
        double b = inputs.get(1);
        return List.of(
                a > b ? 1.0 : 0.0, // A > B
                a < b ? 1.0 : 0.0, // A < B
                Math.abs(a - b) < 1e-5 ? 1.0 : 0.0  // A = B
        );
    }

    public String a(){
        return in(0);
    }

    public String b(){
        return in(1);
    }

    public String ge(){
        return out(0);
    }

    public String le(){
        return out(1);
    }

    public String eq(){
        return out(2);
    }

    public ComponentPortName __a(){
        return __in(0);
    }

    public ComponentPortName __b(){
        return __in(1);
    }

    public ComponentPortName __ge(){
        return __out(0);
    }

    public ComponentPortName __le(){
        return __out(1);
    }

    public ComponentPortName __eq(){
        return __out(2);
    }
}

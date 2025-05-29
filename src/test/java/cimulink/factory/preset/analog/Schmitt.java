package cimulink.factory.preset.analog;

import cimulink.factory.basic.analog.Analog11;
import kotlin.Pair;

import java.util.List;

public class Schmitt extends Analog11<Schmitt.SchmittState> {



    public Schmitt(double up, double down) {
        super(
                Schmitt::transit,
                new SchmittState(false, up, down)
        );
    }

    private static Pair<Double, SchmittState> transit(Pair<Double, SchmittState> in){
        double raw = in.getFirst();
        SchmittState state = in.getSecond();

        double up = state.upValue();
        double down = state.downValue();

        double out = state.up() ?
                raw < down ? down : up
                :
                raw > up ? up : down;

        boolean next_up = state.up() ?
                raw > down
                :
                raw > up;

        return new Pair<>(out, new SchmittState(next_up, up, down));
    }


    public record SchmittState(boolean up, double upValue, double downValue){
    }
}

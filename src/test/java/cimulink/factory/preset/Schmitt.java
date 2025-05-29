package cimulink.factory.preset;

import com.verr1.controlcraft.utils.MathUtils;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class Schmitt extends Component11<Schmitt.SchmittState>{



    public Schmitt(double up, double down) {
        super(
                Schmitt::transit,
                new SchmittState(false, up, down)
        );
    }

    private static Pair<List<Double>, SchmittState> transit(Pair<List<Double>, SchmittState> in){
        double raw = in.getFirst().get(0);
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

        return new Pair<>(List.of(out), new SchmittState(next_up, up, down));
    }


    public record SchmittState(boolean up, double upValue, double downValue){
    }
}

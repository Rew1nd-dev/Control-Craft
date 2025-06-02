package cimulink.v3.components.analog;

import cimulink.v3.components.general.Temporal;
import cimulink.v3.utils.ArrayUtils;
import kotlin.Pair;

import java.util.List;

public class Schmitt extends Temporal<Boolean> {

    private double upThresh = 0;
    private double downThresh = 0;

    public Schmitt(double upThresh, double downThresh) {
        super(ArrayUtils.SINGLE_INPUT, ArrayUtils.SINGLE_OUTPUT, () -> false);
        this.downThresh = downThresh;
        this.upThresh = upThresh;
    }

    @Override
    protected Pair<List<Double>, Boolean> transit(List<Double> input, Boolean up) {
        double in = input.get(0);
        double out = 0;
        boolean nextUp = false;
        if (!up){
            out = in > upThresh ? upThresh : downThresh;
            nextUp = in > upThresh;
        } else {
            out = in < downThresh ? downThresh : upThresh;
            nextUp = in < downThresh;
        }


        return new Pair<>(List.of(out), nextUp);
    }
}

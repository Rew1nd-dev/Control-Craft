package cimulink.factory.preset.digital.ff;

import cimulink.factory.basic.digital.Digital11;
import kotlin.Pair;

import java.util.function.Function;

public class DFlipFlop extends Digital11<Boolean> {


    // DFF has no state, but it has immediate = true, so forward() stops at DFF

    public DFlipFlop() {
        super(DFlipFlop::transition, false);
    }

    private static Pair<Boolean, Boolean> transition(Pair<Boolean, Boolean> input) {
        Boolean d = input.getFirst();  // current input

        return new Pair<>(d, false); // current result, current state
    }

}

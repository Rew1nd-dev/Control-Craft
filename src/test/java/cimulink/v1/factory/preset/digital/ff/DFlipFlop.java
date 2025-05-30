package cimulink.v1.factory.preset.digital.ff;

import cimulink.v1.factory.basic.digital.Digital11;
import kotlin.Pair;

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

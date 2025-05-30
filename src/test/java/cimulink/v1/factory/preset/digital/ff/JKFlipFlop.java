package cimulink.v1.factory.preset.digital.ff;

import cimulink.v1.factory.basic.digital.Digital22;
import kotlin.Pair;

public class JKFlipFlop extends Digital22<Boolean> {


    // TFF has no state, but it has immediate = true, so forward() stops at DFF

    public JKFlipFlop() {
        super(JKFlipFlop::transition, false);
    }

    private static Pair<Pair<Boolean, Boolean>, Boolean> transition(Pair<Pair<Boolean, Boolean>, Boolean> input) {
        Boolean j = input.getFirst().getFirst();  // current input J
        Boolean k = input.getFirst().getSecond(); // current input K
        Boolean q = input.getSecond();             // current state Q

        Pair<Boolean, Boolean> out = new Pair<>(q, !q); // Default output: Q and !Q

        if (j && !k) {
            return new Pair<>(out, true);  // Set
        } else if (!j && k) {
            return new Pair<>(out, false); // Reset
        } else if (j && k) {
            return new Pair<>(out, !q);    // Toggle
        } else {
            return new Pair<>(out, q);     // No change
        }
    }

}

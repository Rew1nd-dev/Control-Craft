package cimulink.v1.factory.basic.digital;

import cimulink.v1.utils.ArrayUtils;
import kotlin.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.function.Function;

public class Digital22<S> extends DigitalNM<S>{

    public Digital22(
            Function<Pair<@NotNull Pair<Boolean, Boolean>, @NotNull S>, Pair<@NotNull Pair<Boolean, Boolean>, @NotNull S>> transition,
            S defaultState
    ) {
        super(
                ArrayUtils.createInputNames(2),
                ArrayUtils.createOutputNames(2),
                arr -> {
                    Pair<Boolean, Boolean> inputPair = new Pair<>(arr.getFirst().get(0), arr.getFirst().get(1));
                    S state = arr.getSecond();
                    Pair<Pair<Boolean, Boolean>, S> result = transition.apply(new Pair<>(inputPair, state));
                    return new Pair<>(
                        List.of(result.getFirst().getFirst(), result.getFirst().getSecond()),
                        result.getSecond()
                    );
                },
                defaultState
        );
    }

    public Digital22(
            Function<Pair<Boolean, Boolean>, Pair<Boolean, Boolean>> transition
    ) {
        super(
                ArrayUtils.createInputNames(2),
                ArrayUtils.createOutputNames(2),
                arr -> {
                    Pair<Boolean, Boolean> inputPair = new Pair<>(arr.get(0), arr.get(1));
                    Pair<Boolean, Boolean> result = transition.apply(inputPair);
                    return List.of(result.getFirst(), result.getSecond());
                }
        );
    }

    public String in(int i){
        if (i < 0 || i >= 2) {
            throw new IndexOutOfBoundsException("Input index must be 0 or 1");
        }
        return ArrayUtils.__in(i);
    }

    public String out(int i){
        if (i < 0 || i >= 2) {
            throw new IndexOutOfBoundsException("Output index must be 0 or 1");
        }
        return ArrayUtils.__out(i);
    }


}

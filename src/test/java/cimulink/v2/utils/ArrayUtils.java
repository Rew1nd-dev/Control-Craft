package cimulink.v2.utils;

import kotlin.Pair;

import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

public class ArrayUtils {
    public static final String INPUT_PREFIX = "i_";
    public static final String INPUT_I = "i";
    public static final String OUTPUT_O = "o";
    public static final String OUTPUT_PREFIX = "o_";

    public static final List<String> SINGLE_OUTPUT = List.of(OUTPUT_O);
    public static final List<String> SINGLE_INPUT = List.of(INPUT_I);


    public static String __in(int index){
        return INPUT_PREFIX + index;
    }

    public static String __o() {
        return OUTPUT_O;
    }

    public static List<String> createInputNames(int n){
        return IntStream.range(0, n).mapToObj(ArrayUtils::__in).toList();
    }

    public static<T> void AssertSize(List<T> array, int size){
        if (array.size() != size) {
            throw new IllegalArgumentException("Expected size: " + size + ", but got: " + array.size());
        }
    }

    public static void AssertRange(int index, int max){
        if (index < 0 || index >= max) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + max);
        }
    }

    public static List<String> createOutputNames(int n){
        return IntStream.range(0, n).mapToObj(ArrayUtils::__out).toList();
    }

    public static String __out(int index){
        return OUTPUT_PREFIX + index;
    }

    public static<T> Function<List<T>, List<T>> wrapCombinational(Function<T, T> transform) {
        return input -> List.of(transform.apply(input.get(0)));
    }

    public static<T, S> Function<Pair<List<T>, S>, Pair<List<T>, S>> wrapTemporal(Function<Pair<T, S>, Pair<T, S>> transform) {
        return input -> {
            Pair<T, S> result = transform.apply(new Pair<>(input.getFirst().get(0), input.getSecond()));
            return new Pair<>(List.of(result.getFirst()), result.getSecond());
        };
    }
}

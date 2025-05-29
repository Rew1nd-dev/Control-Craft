package cimulink.factory.preset;

import cimulink.factory.basic.Component11;
import cimulink.factory.basic.ComponentNM;
import kotlin.Pair;

import java.util.*;
import java.util.stream.IntStream;


public class ShifterNM extends ComponentNM<ShifterNM.RegisterNM> {
    public static final String OUTPUT_PREFIX = "o_";
    public static final String INPUT_PREFIX = "i_";


    // N For Delay, M for Parallel Size
    public ShifterNM(int N, int M) {
        super(
                createInputNames(M),
                createOutputNames(M),
                ShifterNM::transit,
                new RegisterNM(N, M)
        );
    }

    public static Pair<List<Double>, RegisterNM> transit(Pair<List<Double>, RegisterNM> in){
        List<Double> vals = in.getFirst();
        RegisterNM reg = in.getSecond();
        List<Double> out = reg.values();
        reg.shift(vals);
        return new Pair<>(out, reg);
    }

    public static String __in(int index){
        return INPUT_PREFIX + index;
    }

    public String in(int index){
        if(index < 0 || index >= inputs().size()){
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for inputs.");
        }
        return __in(index);
    }

    public static String __out(int index){
        return OUTPUT_PREFIX + index;
    }

    public String out(int index){
        if(index < 0 || index >= outputs().size()){
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for outputs.");
        }
        return __out(index);
    }

    public static List<String> createInputNames(int n){
        return IntStream.range(0, n).mapToObj(ShifterNM::__in).toList();
    }

    public static List<String> createOutputNames(int n){
        return IntStream.range(0, n).mapToObj(ShifterNM::__out).toList();
    }

    public static class RegisterNM {

        private final List<Queue<Double>> storage = new ArrayList<>();
        private final int N;
        private final int M;

        public RegisterNM(int n, int m) {
            N = n;
            M = m;

            for(int i = 0; i < M; i++){
                var dq = new ArrayDeque<Double>(n);
                while (dq.size() < N)dq.add(0.0);
                storage.add(dq);
            }


        }


        public void shift(List<Double> values) {
            if(values.size() != M) {
                throw new IllegalArgumentException("Values size: " + values.size() + " must match the number of registers : " + M);
            }

            for (int i = 0; i < M; i++) {
                Queue<Double> queue = storage.get(i);
                queue.poll(); // Remove the oldest value
                queue.add(values.get(i)); // Add the new value
            }
        }

        public List<Double> values(){
            return storage.stream().map(q -> Optional.ofNullable(q.peek()).orElse(0.0)).toList();
        }

    }
}

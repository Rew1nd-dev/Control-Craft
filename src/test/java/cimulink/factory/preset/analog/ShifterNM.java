package cimulink.factory.preset.analog;

import cimulink.factory.basic.analog.AnalogNM;
import cimulink.utils.ArrayUtils;
import kotlin.Pair;

import java.util.*;


public class ShifterNM extends AnalogNM<ShifterNM.RegisterNM> {

    // N For Delay, M for Parallel Size
    public ShifterNM(int N, int M) {
        super(
                ArrayUtils.createInputNames(M),
                ArrayUtils.createOutputNames(M),
                ShifterNM::transit,
                new RegisterNM(N, M)
        );
    }

    public static Pair<List<Double>, RegisterNM> transit(Pair<List<Double>, RegisterNM> in){
        List<Double> vals = in.getFirst();
        RegisterNM reg = in.getSecond();
        reg.shift(vals);
        List<Double> out = reg.values();
        return new Pair<>(out, reg);
    }



    public String in(int index){
        if(index < 0 || index >= inputs().size()){
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for inputs.");
        }
        return ArrayUtils.__in(index);
    }

    public String out(int index){
        if(index < 0 || index >= outputs().size()){
            throw new IndexOutOfBoundsException("Index " + index + " is out of bounds for outputs.");
        }
        return ArrayUtils.__out(index);
    }



    public static class RegisterNM {

        private final List<Queue<Double>> storage = new ArrayList<>();
        private final int N;
        private final int M;

        public RegisterNM(int n, int m) {
            N = n;
            M = m;

            for(int i = 0; i < M; i++){
                var dq = new ArrayDeque<Double>(N + 2);
                while (dq.size() <= N)dq.add(0.0);
                storage.add(dq);
            }


        }


        public void shift(List<Double> values) {
            if(values.size() != M) {
                throw new IllegalArgumentException("Values size: " + values.size() + " must match the number of registers : " + M);
            }

            for (int i = 0; i < M; i++) {
                Queue<Double> queue = storage.get(i);
                queue.poll();// Remove the oldest value
                queue.add(values.get(i)); // Add the new value
            }
        }

        public List<Double> values(){
            return storage.stream().map(q -> Optional.ofNullable(q.peek()).orElse(0.0)).toList();
        }

    }
}

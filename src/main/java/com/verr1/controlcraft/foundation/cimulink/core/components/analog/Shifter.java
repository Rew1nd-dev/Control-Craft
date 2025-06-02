package com.verr1.controlcraft.foundation.cimulink.core.components.analog;


import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import kotlin.Pair;

import java.util.*;

public class Shifter extends Temporal<Shifter.ShifterQueue> {


    public Shifter(
            int delay,
            int parallel
    ) {
        super(
                ArrayUtils.createInputNames(parallel),
                ArrayUtils.createOutputNames(parallel),
                () -> new ShifterQueue(delay, parallel)
        );
    }

    @Override
    protected Pair<List<Double>, ShifterQueue> transit(List<Double> input, ShifterQueue state) {
        state.shift(input);
        List<Double> out = state.values();
        return new Pair<>(out, state);
    }

    public static class ShifterQueue{
        private final List<Queue<Double>> storage = new ArrayList<>();
        private final int N;
        private final int M;

        public ShifterQueue(int delay, int parallel) {
            N = delay;
            M = parallel;

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

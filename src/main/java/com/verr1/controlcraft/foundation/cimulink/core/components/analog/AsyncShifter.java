package com.verr1.controlcraft.foundation.cimulink.core.components.analog;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import kotlin.Pair;

import java.util.*;

public class AsyncShifter extends Temporal<AsyncShifter.AsyncShifterQueue> {

    public AsyncShifter(
            int delay,
            int parallel
    ) {
        super(
                ArrayUtils.flatten(
                        ArrayUtils.createInputNames(parallel),
                        List.of("clk")
                ),
                ArrayUtils.createOutputNames(parallel),
                () -> new AsyncShifter.AsyncShifterQueue(delay, parallel)
        );
    }

    public int clk(){
        return n() - 1;
    }

    @Override
    protected Pair<List<Double>, AsyncShifter.AsyncShifterQueue> transit(
            List<Double> input,
            AsyncShifter.AsyncShifterQueue state
    ) {
        List<Double> inputs = input.subList(0, n() - 1);
        boolean clk = input.get(clk()) > 0.5;
        boolean lastClk = state.lastClk;
        state.lastClk = clk;
        if(clk && !lastClk){
            state.shift(inputs);
            List<Double> out = state.values();
            return new Pair<>(out, state);
        }


        return new Pair<>(state.values(), state);
    }

    public static class AsyncShifterQueue{
        private final List<Queue<Double>> storage = new ArrayList<>();
        private final int N;
        private final int M;

        private boolean lastClk;

        public AsyncShifterQueue(int delay, int parallel) {
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

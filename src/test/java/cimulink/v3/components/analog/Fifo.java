package cimulink.v3.components.analog;

import cimulink.v3.api.StateFactory;
import cimulink.v3.components.general.Temporal;
import cimulink.v3.utils.ArrayUtils;
import kotlin.Pair;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;

public class Fifo extends Temporal<Fifo.FifoQueue> {


    public Fifo(int n) {
        super(
                ArrayUtils.SINGLE_INPUT,
                ArrayUtils.createOutputNames(n),
                () -> new FifoQueue(n)
        );
    }

    @Override
    protected Pair<List<Double>, FifoQueue> transit(List<Double> input, FifoQueue state) {
        double in = input.get(0);
        state.shift(in);
        List<Double> out = state.values();
        return new Pair<>(out, state);
    }

    public static class FifoQueue {
        private final int N;
        private final Queue<Double> queue;

        public FifoQueue(int size) {
            this.N = size;
            this.queue = new ArrayDeque<>(size);
            while (queue.size() < N) {
                queue.add(0.0); // Initialize with zeros
            }
        }

        public void shift(double value) {
            if (queue.size() >= N) {
                queue.poll(); // Remove the oldest value
            }
            queue.add(value); // Add the new value
        }

        public List<Double> values() {
            return List.copyOf(queue);
        }

    }
}

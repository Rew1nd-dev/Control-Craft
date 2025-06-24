package com.verr1.controlcraft.foundation.cimulink.core.components.sources;

import com.verr1.controlcraft.foundation.cimulink.core.components.general.Combinational;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;

import java.util.List;

public class MultiSink extends Combinational {

    public MultiSink(int n) {
        super(ArrayUtils.ListOf(n, i -> "channel[%d]".formatted(i)), List.of());
    }

    @Override
    protected List<Double> transform(List<Double> inputs) {
        return List.of();
    }
}

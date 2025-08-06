package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.registry.Factory;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.Summary;

public interface ICompilable<T extends NamedComponent> extends ISummarizable{

    T component();

    Factory<T> factory();

    @Override
    default Summary summary() {
        return factory().summarize(component());
    }
}

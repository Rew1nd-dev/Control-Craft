package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.joml.Vector3dc;

import java.util.List;

public class GPSSensor extends NamedComponent {

    private final SensorBlockEntity sp;

    public GPSSensor(SensorBlockEntity sp) {
        super(List.of(), List.of("x", "y", "z"));
        this.sp = sp;
    }


    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();// IntStream.range(0, n()).boxed().toList();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public void onPositiveEdge() {
        Vector3dc position = sp.getSensorPosition();
        updateOutput(List.of(position.x(), position.y(), position.z()));
    }

}

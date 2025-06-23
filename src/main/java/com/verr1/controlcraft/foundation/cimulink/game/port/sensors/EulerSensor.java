package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public class EulerSensor extends NamedComponent {

    private final SensorBlockEntity sp;

    public EulerSensor(SensorBlockEntity sp) {
        super(List.of(), List.of("yaw", "pitch", "roll"));
        this.sp = sp;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }
    // x -- pitch
    // y -- yaw
    // z -- roll
    @Override
    public void onPositiveEdge() {
        Quaterniondc q = sp.getQuaternion();
        Vector3dc e = q.getEulerAnglesYXZ(new Vector3d());
        updateOutput(List.of(e.y(), e.x(), e.z()));
    }
}

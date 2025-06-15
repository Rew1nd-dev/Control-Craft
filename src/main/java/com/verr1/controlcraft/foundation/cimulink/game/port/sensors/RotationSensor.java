package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.VSMathUtils;
import org.joml.*;

import java.util.List;

public class RotationSensor extends NamedComponent {


    private final SensorBlockEntity sp;

    public RotationSensor(SensorBlockEntity sp) {
        super(List.of(), List.of("qx", "qy", "qz", "qw"));
        this.sp = sp;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public void onPositiveEdge() {
        Quaterniondc q = sp.getQuaternion();
        updateOutput(List.of(q.x(), q.y(), q.z(), q.w()));
    }

}

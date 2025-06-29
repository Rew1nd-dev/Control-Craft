package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.joml.Matrix3d;
import org.joml.Matrix3dc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public class VelocitySensor extends NamedComponent {

    private boolean transformToLocal = false;
    private final SensorBlockEntity sp;

    public VelocitySensor(SensorBlockEntity sp) {
        super(List.of(), List.of("vx", "vy", "vz"));
        this.sp = sp;
    }

    public void setLocal(boolean transformToLocal){
        this.transformToLocal = transformToLocal;
    }

    public boolean local(){
        return transformToLocal;
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
        Matrix3dc transform = transformToLocal ? sp.getTw2s() : new Matrix3d();
        Vector3dc velocity = transform.transform(sp.getSensorVelocity(), new Vector3d());
        updateOutput(List.of(velocity.x(), velocity.y(), velocity.z()));
    }

}

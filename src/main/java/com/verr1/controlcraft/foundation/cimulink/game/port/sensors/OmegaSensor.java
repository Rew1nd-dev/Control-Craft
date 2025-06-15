package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.joml.Matrix3d;
import org.joml.Matrix3dc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;
import java.util.stream.IntStream;

public class OmegaSensor extends NamedComponent {

    private boolean transformToLocal = false;
    private final SensorBlockEntity sp;

    public OmegaSensor(SensorBlockEntity sp) {
        super(List.of(), List.of("wx", "wy", "wz"));
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
        Matrix3dc transform = transformToLocal ? sp.getTs2w() : new Matrix3d();
        Vector3dc omega = transform.transform(sp.getAngularVelocity(), new Vector3d());
        updateOutput(List.of(omega.x(), omega.y(), omega.z()));
    }
}

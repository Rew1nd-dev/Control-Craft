package com.verr1.controlcraft.foundation.cimulink.game.port.sensors;

import com.verr1.controlcraft.content.links.sensor.SensorBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.joml.Matrix3d;
import org.joml.Matrix3dc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public class AllInOneSensor extends NamedComponent implements ITransformable{

    private boolean transformToLocal = false;
    private final SensorBlockEntity sp;

    public AllInOneSensor(SensorBlockEntity sp) {
        super(List.of(), List.of(
                    "x", "y", "z",
                    "vx", "vy", "vz",
                    "wx", "wy", "wz",
                    "qx", "qy", "qz", "qw"
            )
        );
        this.sp = sp;
    }

    @Override
    public List<Integer> propagateTo(int inputIndex) {
        return List.of();
    }

    public void setLocal(boolean transformToLocal){
        this.transformToLocal = transformToLocal;
    }

    public boolean local(){
        return transformToLocal;
    }

    @Override
    public void onInputChange(Integer... inputIndexes) {

    }

    @Override
    public void onPositiveEdge() {
        Matrix3dc transform = transformToLocal ? sp.getTw2s() : new Matrix3d();
        Vector3dc velocity = transform.transform(sp.getSensorVelocity(), new Vector3d());
        Vector3dc position = sp.getSensorPosition();
        Vector3dc angularVelocity = transform.transform(sp.getAngularVelocity(), new Vector3d());
        var q = sp.getQuaternion();
        updateOutput(List.of(
                position.x(), position.y(), position.z(),
                velocity.x(), velocity.y(), velocity.z(),
                angularVelocity.x(), angularVelocity.y(), angularVelocity.z(),
                q.x(), q.y(), q.z(), q.w()
        ));
    }
}

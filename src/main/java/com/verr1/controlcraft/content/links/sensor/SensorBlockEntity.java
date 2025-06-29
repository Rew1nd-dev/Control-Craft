package com.verr1.controlcraft.content.links.sensor;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.sensors.SensorLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.SensorTypes;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.ShipPhysics;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.*;

import java.util.Optional;

public class SensorBlockEntity extends CimulinkBlockEntity<SensorLinkPort> {

    public static final NetworkKey SENSOR = NetworkKey.create("sensor_type");
    public static final NetworkKey LOCAL = NetworkKey.create("local_vector");

    public SensorBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(SENSOR)
                .withBasic(SerializePort.of(
                        () -> linkPort().getCurrentType(),
                        t -> linkPort().setCurrentType(t),
                        SerializeUtils.ofEnum(SensorTypes.class)
                ))
                .withClient(ClientBuffer.of(SensorTypes.class))
                .runtimeOnly()
                .register();

        buildRegistry(LOCAL)
                .withBasic(SerializePort.of(
                        () -> linkPort().local(),
                        t -> linkPort().setLocal(t),
                        SerializeUtils.BOOLEAN
                ))
                .withClient(ClientBuffer.BOOLEAN.get())
                .runtimeOnly()
                .register();
    }

    public Quaterniondc getQuaternion(){
        return readSelf().quaternion();
    }

    public Vector3dc getSensorVelocity(){
        return Optional
                .ofNullable(getShipOn())
                .map(ship ->
                {
                    ShipPhysics p = readSelf();
                    Vector3dc sv_wc = p.velocity();
                    Vector3dc sw_wc = p.omega();

                    Vector3dc s_sc = p.positionInShip();
                    Vector3dc p_sc = ValkyrienSkies.set(new Vector3d(), getBlockPos().getCenter());
                    Vector3dc r_sc = new Vector3d(p_sc).sub(s_sc);

                    Vector3dc r_wc = p.s2wTransform().transformDirection(r_sc, new Vector3d());
                    return new Vector3d(sv_wc).add(new Vector3d(sw_wc).cross(r_wc));
                })
                .orElse(new Vector3d());

    }

    public Vector3dc getAngularVelocity(){
        return readSelf().omega();
    }

    public Matrix3dc getTs2w(){
        return readSelf().rotationMatrix();
    }

    public Matrix3dc getTw2s(){
        return readSelf().rotationMatrix().transpose(new Matrix3d());
    }

    @Override
    protected SensorLinkPort create() {
        return new SensorLinkPort(this);
    }
}

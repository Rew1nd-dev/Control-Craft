package com.verr1.controlcraft.content.links.proxy;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.port.plant.PlantProxyLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.ProxyPortStatus;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class ProxyLinkBlockEntity extends CimulinkBlockEntity<PlantProxyLinkPort> {

    public static final NetworkKey ALL_STATUS = NetworkKey.create("proxy_all_status");
    // public static final NetworkKey OUT_STATUS = NetworkKey.create("proxy_out_status");
    public static final SerializeUtils.Serializer<ProxyPortStatus> PROXY_PORT =
            SerializeUtils.of(
                    ProxyPortStatus::serialize,
                    ProxyPortStatus::deserialize
            );


    public ProxyLinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(ALL_STATUS)
                .withBasic(SerializePort.of(
                        () -> linkPort().viewAll(),
                        s -> linkPort().setAll(s),
                        PROXY_PORT
                ))
                .withClient(new ClientBuffer<>(PROXY_PORT, ProxyPortStatus.class))
                .register();

    }


    public void updateAttachedPlant(){
        NamedComponent plant =
        BlockEntityGetter.getLevelBlockEntityAt(
                    level,
                    getBlockPos().relative(getDirection().getOpposite()),
                    IPlant.class
                )
                .map(IPlant::plant)
                .orElse(null);

        linkPort().setPlant(plant);
    }



    @Override
    protected PlantProxyLinkPort create() {
        return new PlantProxyLinkPort();
    }
}

package com.verr1.controlcraft.content.links.proxy;

import com.verr1.controlcraft.content.compact.tweak.TweakControllerCompact;
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
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

public class ProxyLinkBlockEntity extends CimulinkBlockEntity<PlantProxyLinkPort> {

    public static final NetworkKey ALL_STATUS = NetworkKey.create("proxy_all_status");
    // public static final NetworkKey OUT_STATUS = NetworkKey.create("proxy_out_status");



    public ProxyLinkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(ALL_STATUS)
                .withBasic(SerializePort.of(
                        () -> linkPort().viewAll(),
                        s -> linkPort().setAll(s),
                        PlantProxyLinkPort.PROXY_PORT
                ))
                .runtimeOnly()
                .withClient(new ClientBuffer<>(PlantProxyLinkPort.PROXY_PORT, ProxyPortStatus.class))
                .register();

    }

    // no need to call this at initialize(), since it will be called at linkPort()
    public void updateAttachedPlant(){
        if(!(level instanceof ServerLevel serverLevel))return;
        NamedComponent plant =
        BlockEntityGetter.getLevelBlockEntityAt(
                    level,
                    getBlockPos().relative(getDirection().getOpposite()),
                    IPlant.class
                )
                .map(IPlant::plant)
                .orElseGet(() ->
                    TweakControllerCompact.tweakedControllerPlant(
                        serverLevel,
                        getBlockPos().relative(getDirection().getOpposite()))
                );

        linkPort().setPlant(plant);
    }



    @Override
    protected PlantProxyLinkPort create() {
        return new PlantProxyLinkPort(this);
    }
}

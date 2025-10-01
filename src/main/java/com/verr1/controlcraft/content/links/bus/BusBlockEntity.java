package com.verr1.controlcraft.content.links.bus;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.BusLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.IBusContext;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

import java.util.Optional;
import java.util.Set;

public class BusBlockEntity extends CimulinkBlockEntity<BusLinkPort> implements IBusContext {

    private String name = "Bus";
    public static final NetworkKey STATUS = NetworkKey.create("bus_status");

    public BusBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        buildRegistry(STATUS)
                .withBasic(SerializePort.of(
                        () -> linkPort().getStatus(),
                        s -> linkPort().updateStatus(s),
                        BusLinkPort.SER
                ))
                .withClient(new ClientBuffer<>(
                        BusLinkPort.SER,
                        BusLinkPort.Status.class
                ))
                .register();
    }

    @Override
    protected BusLinkPort create() {
        return new BusLinkPort(this);
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        linkPort().updateCache();
    }

    @Override
    public String receiverName() {
        return name;
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    protected Optional<CimulinkBus> bus(){
        return Optional.ofNullable(getLoadedServerShip())
                .map(CimulinkBus::getOrCreate);
    }

    @Override
    public @NotNull Set<NamedComponent> access(String name) {
        return bus().map(b -> b.access(name)).orElse(Set.of());
    }

    @Override
    public @NotNull Set<String> allNames() {
        return bus().map(CimulinkBus::allNames).orElse(Set.of());
    }
}

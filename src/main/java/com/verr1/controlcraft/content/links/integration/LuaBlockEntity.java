package com.verr1.controlcraft.content.links.integration;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.foundation.cimulink.core.api.IBusAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.IBusContext;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.LuacuitLinkPort;
import com.verr1.controlcraft.utils.ConstraintClusterUtil;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LuaBlockEntity extends WirelessIntegrationBlockEntity<Luacuit, LuacuitLinkPort> implements
    IBusContext
{

    private final Map<String, Set<NamedComponent>> cache = new ConcurrentHashMap<>();

    public LuaBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }

    @Override
    protected LuacuitLinkPort create() {
        return new LuacuitLinkPort();
    }

    public void setLuaToWorldAccess() {
        linkPort().setPhysAccess(IPhysAccess.of(this));
        linkPort().setWorldAccess(IWorldAccess.of(this));
        linkPort().setBusAccess(IBusAccess.of(this));
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        setLuaToWorldAccess();
        updateCache();
    }

    private void updateCache(){
        cache.clear();
        allNames().forEach(name -> {
            cache.put(name, access(name));
        });
    }

    public void loadCircuit(LuacuitScript nbt) throws IllegalArgumentException {
        var savedStatus = linkPort().viewStatus();
        boolean shouldOpen = linkPort().isEmpty();
        try {
            linkPort().load(nbt);
        } catch (IllegalArgumentException e) {
            setChanged();
            throw e;
        }
        linkPort().setStatus(savedStatus);
        if (shouldOpen)
            linkPort().setToAllOpen();
        updateIOName();
        setChanged();
    }

    protected Optional<CimulinkBus> bus(){
        return Optional.ofNullable(getLoadedServerShip())
            .map(CimulinkBus::getOrCreate);
    }

    protected Stream<CimulinkBus> buses(){
        return ConstraintClusterUtil.clusterOf(getShipOrGroundID())
            .stream()
            .map(ConstraintClusterUtil::getShipOf)
            .filter(Optional::isPresent)
            .flatMap(Optional::stream)
            .map(CimulinkBus::getOrCreate)
            ;
    }

    public void propagateTo(String name, String port, double value){
        cache.getOrDefault(name, Set.of()).forEach(component -> {
            if(component.hasInput(port)){
                component.input(port, value);
            }
        });
    }

    public double retrieveFrom(String name, String port){
        return
            cache.getOrDefault(name, Set.of()).stream()
            .filter(nc -> nc.hasOutput(port))
            .findFirst()
            .map(nc -> {
                // double check
                if(nc.hasOutput(port)){
                    return nc.peekOutput(port);
                }
                return 0.0;
            })
            .orElse(0.0);
    }

    // just like what BusBlockEntity does
    // This is call delegate from Luacuit onPositiveEdge
    public void onPositiveEdge(){
//        try{
//            cache.values().stream().flatMap(Collection::stream).forEach(c -> {
//                try{
//                    c.onPositiveEdge();
//                }catch (RuntimeException e){
//                    ControlCraft.LOGGER.error("Error During Temporal Propagation At : {}, {}", c.getClass(), e.getMessage());
//                    throw e;
//                }
//            });
//        } catch (RuntimeException e) {
//            ControlCraft.LOGGER.error("Error During Temporal Propagation At LuaBlock: {}, {}", e.getCause(), e.getMessage());
//            throw new RuntimeException(e);
//        }
    }

    @Override
    public @NotNull Set<NamedComponent> access(String name) {
        return buses().flatMap(b -> b.access(name).stream()).collect(Collectors.toSet());
    }

    @Override
    public @NotNull Set<String> allNames() {
        return buses().flatMap(b -> b.allNames().stream()).collect(Collectors.toSet());
    }
}

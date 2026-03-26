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
import com.verr1.controlcraft.utils.ConstraintClusterBusResolver;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

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

    public void propagateTo(String name, String port, double value){
        ConstraintClusterBusResolver.propagate(getShipOrGroundID(), name, port, value);
    }

    public double retrieveFrom(String name, String port){
        return ConstraintClusterBusResolver.retrieve(getShipOrGroundID(), name, port);
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
        return ConstraintClusterBusResolver.access(getShipOrGroundID(), name);
    }

    @Override
    public @NotNull Set<String> allNames() {
        return ConstraintClusterBusResolver.allNames(getShipOrGroundID());
    }
}

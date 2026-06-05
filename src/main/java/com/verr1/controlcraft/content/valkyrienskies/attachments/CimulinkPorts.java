package com.verr1.controlcraft.content.valkyrienskies.attachments;


import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.links.computer.ComputerBlockEntity;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.LazyTicker;
import net.minecraft.world.level.block.entity.BlockEntity;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.ValkyrienSkiesMod;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@JsonAutoDetect(
        fieldVisibility = JsonAutoDetect.Visibility.ANY,
        getterVisibility = JsonAutoDetect.Visibility.NONE,
        isGetterVisibility = JsonAutoDetect.Visibility.NONE,
        setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public final class CimulinkPorts {
    @JsonIgnore
    private static final LazyTicker TICKER = new LazyTicker(20, CimulinkPorts::tickAllAttachments);

    public static CimulinkPorts getOrCreate(AttachmentHolder ship){
        return ship.getOrPutAttachment(CimulinkPorts.class, CimulinkPorts::new);
    }

    public static @Nullable CimulinkPorts get(AttachmentHolder ship) {
        return ship.getAttachment(CimulinkPorts.class);
    }

    public static void tick() {
        TICKER.tick();
    }

    public static void tickAllAttachments() {
        VSGameUtilsKt.getShipObjectWorld(ControlCraftServer.INSTANCE)
                .getLoadedShips()
                .stream()
                .map(CimulinkPorts::get)
                .filter(Objects::nonNull)
                .forEach(CimulinkPorts::cleanupRemovedComputers);
    }


    @JsonIgnore
    private final Map<WorldBlockPos, String> ports = new HashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, ComputerBlockEntity> computers = new ConcurrentHashMap<>();


    public Set<WorldBlockPos> getAll(){
        return Collections.unmodifiableSet(ports.keySet());
    }

    public void set(WorldBlockPos pos, String name){
        ports.put(pos, name);
    }

    public List<WorldBlockPos> getLinksOf(String name){
        return ports.entrySet().stream().filter(e -> e.getValue().equals(name)).map(Map.Entry::getKey).toList();
    }

    public void remove(WorldBlockPos pos){
        ports.remove(pos);
    }

    public void putComputer(WorldBlockPos pos, ComputerBlockEntity computer) {
        computers.put(pos, computer);
    }

    public void removeComputer(WorldBlockPos pos) {
        computers.remove(pos);
    }

    public List<ComputerBlockEntity> getComputersOf(String deviceName) {
        return computers.values().stream()
                .filter(Objects::nonNull)
                .filter(be -> !be.isRemoved())
                .filter(be -> Objects.equals(be.deviceName(), deviceName))
                .toList();
    }

    public void cleanupRemovedComputers() {
        computers.entrySet().removeIf(entry -> {
            BlockEntity blockEntity = entry.getValue();
            return blockEntity == null || blockEntity.isRemoved();
        });
    }


}
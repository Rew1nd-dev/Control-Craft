package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.collect.Sets;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.port.bus.IBusContext;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.utils.LazyTicker;
import com.verr1.controlcraft.utils.VSAccessUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@JsonAutoDetect(
        fieldVisibility = JsonAutoDetect.Visibility.ANY,
        getterVisibility = JsonAutoDetect.Visibility.NONE,
        isGetterVisibility = JsonAutoDetect.Visibility.NONE,
        setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CimulinkBus implements IBusContext {
    @JsonIgnore
    private final Map<String, Set<WorldBlockPos>> addresses = new ConcurrentHashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, String> names = new ConcurrentHashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, Integer> lives = new ConcurrentHashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, NamedComponent> devices = new ConcurrentHashMap<>();
    @JsonIgnore
    private final static int MAX_LIVES = 10;
    @JsonIgnore
    private final static LazyTicker ticker = new LazyTicker(5, CimulinkBus::tickAllAttachments);

    public void activate(@NotNull WorldBlockPos address, @NotNull NamedComponent device, @NotNull String name){
        lives.put(address, MAX_LIVES);
        devices.put(address, device);

        replace(name, address);
    }

    public @NotNull Set<WorldBlockPos> allPositions(){
        return Set.copyOf(names.keySet());
    }

    public void tick(){
        tickLives();
    }

    public @NotNull Set<NamedComponent> access(String name){
        return Optional
                .ofNullable(addresses.get(name))
                .map(set -> set.stream()
                        .map(devices::get)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toSet())
                )
                .orElseGet(Sets::newConcurrentHashSet);
    }


    public void onPositiveEdge(){

        devices.values().forEach(c -> {
            try{
                c.onPositiveEdge();
            } catch (RuntimeException e) {
                ControlCraft.LOGGER.error("Error During Temporal Propagation For {} At CimulinkBus: {}, {}", c.getClass(), e.getCause(), e.getMessage());
                throw new RuntimeException(e);
            }
        });



    }

    public @NotNull Set<String> allNames(){
        return Set.copyOf(addresses.keySet());
    }

    private void tickLives(){
        lives.entrySet().forEach(e -> e.setValue(e.getValue() - 1));
        lives.entrySet().stream().filter(e -> e.getValue() < 0).toList().forEach(e -> {
            remove(e.getKey());
            devices.remove(e.getKey());
            lives.remove(e.getKey());
        });
    }



    private void replace(@NotNull String name, @NotNull WorldBlockPos address){
        String original = names.get(address);
        if(original != null && !original.equals(name)){
            remove(original, address);
        }
        put(name, address);
    }

    private void put(@NotNull String name, @NotNull  WorldBlockPos address){
        names.put(address, name);
        addresses.computeIfAbsent(name, $ -> Sets.newConcurrentHashSet()).add(address);
    }

    private void remove(String name, WorldBlockPos address){
        names.remove(address);
        Set<WorldBlockPos> sets = addresses.get(name);
        if(sets == null){
            return;
        }
        sets.remove(address);
        if(sets.isEmpty())addresses.remove(name);
    }

    private void remove(WorldBlockPos address){
        String name = names.get(address);
        if(name == null)return;
        Set<WorldBlockPos> sets = addresses.get(name);
        if(sets == null)return;
        sets.remove(address);
        if(sets.isEmpty())addresses.remove(name);

    }


    public static CimulinkBus getOrCreate(ServerShip ship){
        //return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
        var obj = ship.getAttachment(CimulinkBus.class);
        if(obj == null){
            obj = new CimulinkBus();
            ship.saveAttachment(CimulinkBus.class, obj);
        }
        return obj;
    }

    public static @Nullable CimulinkBus get(ServerShip ship){
        //return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
        return ship.getAttachment(CimulinkBus.class);
    }

    public static void tickAll(){
        ticker.tick();
    }

    public static void tickAllAttachments(){
        VSAccessUtils
                .getAllShips()
                .stream()
                .map(s -> s.getAttachment(CimulinkBus.class))
                .filter(Objects::nonNull)
                .forEach(CimulinkBus::tick);
    }

}

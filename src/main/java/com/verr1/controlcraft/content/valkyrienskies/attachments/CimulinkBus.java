package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.google.common.collect.Sets;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@JsonAutoDetect(
        fieldVisibility = JsonAutoDetect.Visibility.ANY,
        getterVisibility = JsonAutoDetect.Visibility.NONE,
        isGetterVisibility = JsonAutoDetect.Visibility.NONE,
        setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CimulinkBus {
    @JsonIgnore
    private final Map<String, Set<WorldBlockPos>> addresses = new ConcurrentHashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, String> names = new ConcurrentHashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, Integer> lives = new ConcurrentHashMap<>();
    @JsonIgnore
    private final Map<WorldBlockPos, NamedComponent> devices = new ConcurrentHashMap<>();

    private final static int MAX_LIVES = 10;

    public void activate(WorldBlockPos address, NamedComponent device, String name){
        lives.put(address, MAX_LIVES);
        devices.put(address, device);

        replace(name, address);
    }

    public void tick(){
        tickLives();
    }

    public Set<NamedComponent> access(String name){
        return Optional
                .ofNullable(addresses.get(name))
                .map(set -> set.stream()
                        .map(devices::get)
                        .filter(Objects::nonNull)
                        .collect(Collectors.toSet())
                )
                .orElseGet(Sets::newConcurrentHashSet);
    }


    private void tickLives(){
        lives.entrySet().forEach(e -> e.setValue(e.getValue() - 1));
        lives.entrySet().stream().filter(e -> e.getValue() < 0).toList().forEach(e -> {
            remove(e.getKey());
            devices.remove(e.getKey());
            lives.remove(e.getKey());
        });
    }



    private void replace(String name, WorldBlockPos address){
        String original = names.get(address);
        if(!original.equals(name)){
            remove(original, address);
        }
        put(original, address);
    }

    private void put(String name, WorldBlockPos address){
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

}

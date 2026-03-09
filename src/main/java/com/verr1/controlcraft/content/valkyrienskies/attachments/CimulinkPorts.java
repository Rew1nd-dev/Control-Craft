package com.verr1.controlcraft.content.valkyrienskies.attachments;


import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.*;

@JsonAutoDetect(
        fieldVisibility = JsonAutoDetect.Visibility.ANY,
        getterVisibility = JsonAutoDetect.Visibility.NONE,
        isGetterVisibility = JsonAutoDetect.Visibility.NONE,
        setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CimulinkPorts {


    public static CimulinkPorts getOrCreate(ServerShip ship){
        //return ship.getOrPutAttachment(AnchorForceInducer.class, AnchorForceInducer::new);
        var obj = ship.getAttachment(CimulinkPorts.class);
        if(obj == null){
            obj = new CimulinkPorts();
            ship.saveAttachment(CimulinkPorts.class, obj);
        }
        return obj;
    }


    @JsonIgnore
    private final Map<WorldBlockPos, String> ports = new HashMap<>();


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

    public void validate(){
        ports.keySet().removeIf(w -> BlockLinkPort.of(w).isEmpty());
    }


}

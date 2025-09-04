package com.verr1.controlcraft.content.valkyrienskies.attachments;


import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.HashSet;
import java.util.Set;

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
    private final Set<WorldBlockPos> ports = new HashSet<>();


    public Set<WorldBlockPos> getAll(){
        return Set.copyOf(ports);
    }

    public void add(WorldBlockPos pos){
        ports.add(pos);
    }

    public void remove(WorldBlockPos pos){
        ports.remove(pos);
    }

    public void validate(){
        ports.removeIf(w -> BlockLinkPort.of(w).isEmpty());
    }


}

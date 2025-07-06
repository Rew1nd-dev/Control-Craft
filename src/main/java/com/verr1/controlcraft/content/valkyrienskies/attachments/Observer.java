package com.verr1.controlcraft.content.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.foundation.data.*;
import kotlin.jvm.functions.Function1;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.attachment.AttachmentHolder;
import org.valkyrienskies.core.api.ships.PhysShip;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.api.ships.ShipForcesInducer;
import org.valkyrienskies.core.api.ships.ShipPhysicsListener;
import org.valkyrienskies.core.api.world.PhysLevel;

import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Supplier;

@JsonAutoDetect(
        fieldVisibility = JsonAutoDetect.Visibility.ANY,
        getterVisibility = JsonAutoDetect.Visibility.NONE,
        isGetterVisibility = JsonAutoDetect.Visibility.NONE,
        setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public final class Observer implements ShipPhysicsListener {
    @JsonIgnore
    private final ConcurrentHashMap<WorldBlockPos, ExpirableListener<ShipPhysics>> Listener = new ConcurrentHashMap<>();
    @JsonIgnore
    private final SynchronizedField<ShipPhysics> Observation = new SynchronizedField<>(ShipPhysics.EMPTY);

    @Override
    public void physTick(@NotNull PhysShip physShip, @NotNull PhysLevel physLevel) {
        ShipPhysics tickPhysics = ShipPhysics.of(physShip);
        Observation.write(tickPhysics);
        Listener.values().forEach(listener -> listener.accept(tickPhysics));
        Listener.values().forEach(ExpirableListener::tick);
        Listener.entrySet().removeIf(entry -> entry.getValue().isExpired());
    }



    public static Observer getOrCreate(AttachmentHolder ship){
       return ship.getOrPutAttachment(Observer.class, Observer::new);
    }


    public void replace(WorldBlockPos pos, ExpirableListener<ShipPhysics> listener){
        Listener.put(pos, listener);
        alive(pos);
    }

    private void alive(WorldBlockPos pos){
        Optional.ofNullable(Listener.get(pos)).ifPresent(ExpirableListener::reset);
    }

    public ShipPhysics read(){
        return Observation.read();
    }


}

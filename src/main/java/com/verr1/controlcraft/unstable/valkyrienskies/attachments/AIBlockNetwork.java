package com.verr1.controlcraft.unstable.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.unstable.ai.api.IAIListener;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.HashMap;

@JsonAutoDetect(
        fieldVisibility = JsonAutoDetect.Visibility.ANY,
        getterVisibility = JsonAutoDetect.Visibility.NONE,
        isGetterVisibility = JsonAutoDetect.Visibility.NONE,
        setterVisibility = JsonAutoDetect.Visibility.NONE
)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AIBlockNetwork implements IAIListener{
    @JsonIgnore
    private final HashMap<WorldBlockPos, IAIListener> aiEventListeners = new HashMap<>();

    @JsonIgnore
    private final HashMap<WorldBlockPos, Integer> blockLives = new HashMap<>();

    public void tick(){
        blockLives.entrySet().forEach(e -> e.setValue(e.getValue() - 1));
        blockLives
                .entrySet()
                .stream()
                .filter(e -> e.getValue() < 0)
                .toList()
                .forEach(e -> remove(e.getKey()));
    }

    private void remove(WorldBlockPos wbp){
        blockLives.remove(wbp);
        aiEventListeners.remove(wbp);
    }

    private void activate(WorldBlockPos key){
        blockLives.put(key, 3);
    }

    public void activateListener(WorldBlockPos key, IAIListener listener){
        aiEventListeners.put(key, listener);
        activate(key);
    }

    public static AIBlockNetwork getOrCreate(ServerShip ship){
        var obj = ship.getAttachment(AIBlockNetwork.class);
        if(obj == null){
            obj = new AIBlockNetwork();
            ship.saveAttachment(AIBlockNetwork.class, obj);
        }
        return obj;
    }

    @Override
    public void onDiscard() {
        aiEventListeners.values().forEach(IAIListener::onDiscard);
    }

    @Override
    public void onPostRepair() {
        aiEventListeners.values().forEach(IAIListener::onPostRepair);
    }

    @Override
    public void onPreRepair() {
        aiEventListeners.values().forEach(IAIListener::onPreRepair);
    }

    @Override
    public void onProjectileImpact() {
        aiEventListeners.values().forEach(IAIListener::onProjectileImpact);
    }

    @Override
    public void onSpawn() {
        aiEventListeners.values().forEach(IAIListener::onSpawn);
    }
}

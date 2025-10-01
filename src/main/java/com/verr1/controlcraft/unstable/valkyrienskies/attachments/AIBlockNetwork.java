package com.verr1.controlcraft.unstable.valkyrienskies.attachments;

import com.fasterxml.jackson.annotation.JsonAutoDetect;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IAIListener;
import com.verr1.controlcraft.unstable.blocks.AiBoundFakePlayer;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.HashMap;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

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
    private final HashMap<WorldBlockPos, Object> blockObjects = new HashMap<>();
    @JsonIgnore
    private final HashMap<WorldBlockPos, Integer> blockLives = new HashMap<>();
    @JsonIgnore
    private AiBoundFakePlayer fp = null;

    public void tick(){
        blockLives.entrySet().forEach(e -> e.setValue(e.getValue() - 1));
        blockLives
                .entrySet()
                .stream()
                .filter(e -> e.getValue() < 0)
                .toList()
                .forEach(e -> remove(e.getKey()));
        tickFakePlayer();
    }

    public AiBoundFakePlayer getOrCreateFakePlayer(Supplier<AiBoundFakePlayer> factory){
        if(this.fp == null)this.fp = factory.get();
        return this.fp;
    }

    public void tickFakePlayer(){
        if(fp == null || fp.isInPool())return;
        fp.activate();
    }

    private void remove(WorldBlockPos wbp){
        blockLives.remove(wbp);
        aiEventListeners.remove(wbp);
        blockObjects.remove(wbp);
    }

    private void activate(WorldBlockPos key){
        blockLives.put(key, 3);
    }

    public void activateListener(WorldBlockPos key, IAIListener listener){
        aiEventListeners.put(key, listener);
        activate(key);
    }

    public void activateObject(WorldBlockPos key, Object obj){
        blockObjects.put(key, obj);
        activate(key);
    }

    public<T> void forEachObject(Class<T> type, Consumer<T> operation){
        blockObjects.values().stream()
                .filter(type::isInstance)
                .map(type::cast)
                .forEach(operation);
    }

    public static AIBlockNetwork getOrCreate(ServerShip ship){
        var obj = ship.getAttachment(AIBlockNetwork.class);
        if(obj == null){
            obj = new AIBlockNetwork();
            ship.saveAttachment(AIBlockNetwork.class, obj);
        }
        return obj;
    }

    public static @Nullable AIBlockNetwork get(ServerShip ship){
        return ship.getAttachment(AIBlockNetwork.class);
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
    public void onPreRestore() {
        aiEventListeners.values().forEach(IAIListener::onPreRestore);
    }

    @Override
    public void onPostRestore() {
        aiEventListeners.values().forEach(IAIListener::onPostRestore);
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

package com.verr1.controlcraft.foundation.redstone;

import com.simibubi.create.Create;

import com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler;
import com.simibubi.create.foundation.utility.Couple;
import com.simibubi.create.foundation.utility.WorldHelper;
import net.minecraft.world.level.LevelAccessor;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

import static com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler.withinRange;

public class RedstoneLinkNetworkHandlerExtension {

    static final Map<LevelAccessor, Map<Couple<RedstoneLinkNetworkHandler.Frequency>, Set<$IRedstoneLinkable>>> connections =
            new IdentityHashMap<>();

    public final AtomicInteger globalPowerVersion = new AtomicInteger();


    public void onLoadWorld(LevelAccessor world) {
        connections.put(world, new HashMap<>());
        Create.LOGGER.debug("Prepared Redstone Network Space for {}", WorldHelper.getDimensionID(world));
    }

    public void onUnloadWorld(LevelAccessor world) {
        connections.remove(world);
        Create.LOGGER.debug("Removed Redstone Network Space for {}", WorldHelper.getDimensionID(world));
    }

    public Set<$IRedstoneLinkable> getNetworkOf(LevelAccessor world, $IRedstoneLinkable actor) {
        Map<Couple<RedstoneLinkNetworkHandler.Frequency>, Set<$IRedstoneLinkable>> networksInWorld = networksIn(world);
        Couple<RedstoneLinkNetworkHandler.Frequency> key = actor.getNetworkKey();
        if (!networksInWorld.containsKey(key))
            networksInWorld.put(key, new LinkedHashSet<>());
        return networksInWorld.get(key);
    }

    public void addToNetwork(LevelAccessor world, $IRedstoneLinkable actor) {
        getNetworkOf(world, actor).add(actor);
        updateNetworkOf(world, actor);
    }

    public void removeFromNetwork(LevelAccessor world, $IRedstoneLinkable actor) {
        Set<$IRedstoneLinkable> network = getNetworkOf(world, actor);
        network.remove(actor);
        if (network.isEmpty()) {
            networksIn(world).remove(actor.getNetworkKey());
            return;
        }
        updateNetworkOf(world, actor);
    }

    public void updateNetworkOf(LevelAccessor world, $IRedstoneLinkable actor) {
        Set<$IRedstoneLinkable> network = getNetworkOf(world, actor);
        globalPowerVersion.incrementAndGet();


        network.stream().filter(o -> !o.isAlive()).toList().forEach(network::remove);

        double power = network.stream()
                .filter(other -> other.isSource() && other.isAlive() && withinRange(other, actor))
                .map($IRedstoneLinkable::$getTransmittedStrength)
                .max(Double::compare)
                .orElse(0.0);


        for ($IRedstoneLinkable other : network) {
            if (other != actor && other.isListening() && withinRange(actor, other))
                other.$setReceivedStrength(power);
        }
    }



    public Map<Couple<RedstoneLinkNetworkHandler.Frequency>, Set<$IRedstoneLinkable>> networksIn(LevelAccessor world) {
        if (!connections.containsKey(world)) {
            Create.LOGGER.warn("Tried to Access unprepared network space of " + WorldHelper.getDimensionID(world));
            return new HashMap<>();
        }
        return connections.get(world);
    }

    public boolean hasAnyLoadedPower(Couple<RedstoneLinkNetworkHandler.Frequency> frequency) {
        for (Map<Couple<RedstoneLinkNetworkHandler.Frequency>, Set<$IRedstoneLinkable>> map : connections.values()) {
            Set<$IRedstoneLinkable> set = map.get(frequency);
            if (set == null || set.isEmpty())
                continue;
            for ($IRedstoneLinkable link : set)
                if (link.$getTransmittedStrength() > 0)
                    return true;
        }
        return false;
    }

}

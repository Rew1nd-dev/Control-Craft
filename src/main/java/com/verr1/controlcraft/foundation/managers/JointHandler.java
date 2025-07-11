package com.verr1.controlcraft.foundation.managers;

import com.google.common.collect.Maps;
import com.google.common.collect.Queues;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.constraint.ConstraintKey;
import com.verr1.controlcraft.foundation.data.constraint.ConstraintWithID;
import com.verr1.controlcraft.foundation.data.constraint.SavedConstraintObject;
import com.verr1.controlcraft.foundation.executor.Executor;
import com.verr1.controlcraft.utils.VSGetterUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.events.PhysTickEvent;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.api.world.PhysLevel;
import org.valkyrienskies.core.apigame.joints.VSJoint;
import org.valkyrienskies.core.apigame.joints.VSJointAndId;
import org.valkyrienskies.core.apigame.world.PhysLevelCore;
import org.valkyrienskies.mod.api.ValkyrienSkies;
import org.valkyrienskies.mod.common.ValkyrienSkiesMod;
import org.valkyrienskies.mod.common.util.GameToPhysicsAdapter;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;


public class JointHandler {

    public static final Map<String, JointHandler> DIMENSION_HANDLERS = Maps.newConcurrentMap();
    // For save-load and direct retrieve Joints
    private static final Map<ConstraintKey, ConstraintWithID> cache = Maps.newConcurrentMap();


    public static JointHandler getOrCreate(String dimensionId){
        return DIMENSION_HANDLERS.computeIfAbsent(dimensionId, id -> new JointHandler(dimensionId));
    }

    public static void dispatchEvent(PhysTickEvent event){
        getOrCreate(event.getWorld().getDimension()).physTick(event.getWorld(), event.getDelta());
    }

    public static void onServerStaring(MinecraftServer server){
        cache.clear();

        ConstraintSavedData loadedStorage = ConstraintSavedData.load(server);
        List<SavedConstraintObject> constraintList =
                loadedStorage.data
                        .entrySet()
                        .stream()
                        .map(entry -> new SavedConstraintObject(entry.getKey(), entry.getValue()))
                        .toList();

        constraintList.stream()
                .map(sco -> new request(sco.key(), sco.getConstraint(), Operation.CREATE_OR_UPDATE))
                .filter(r -> r.joint != null && r.key != null)
                .forEach(r -> requestOverrideJoint(r.key, r.joint));

    }

    public static void onServerStopping(MinecraftServer $){
        saveAll();
    }

    public static void removeInvalidJoints(){
        DIMENSION_HANDLERS.values().forEach(jh -> jh.removeInvalidCheck = true);
    }

    public static void saveAll(){
        ConstraintSavedData storage = ConstraintSavedData.load(ControlCraftServer.INSTANCE);
        storage.clear();
        ControlCraft.LOGGER.info("Saving {} constrains", cache.size());
        cache.forEach((key, data) -> {
            if(key.runtimeOnly())return;
            try{
                storage.put(key, data.constrain());
            }catch (Exception e){
                ControlCraft.LOGGER.error("Failed to save constrain", e);
            }
        });
    }

    public static @Nullable VSJoint retrieveJoint(ConstraintKey key){
        return cache.containsKey(key) ? cache.get(key).constrain() : null;
    }

    public static void requestOverrideJoint(ConstraintKey key, VSJoint joint){
        getOrCreate(key.dimension()).requestOverride(key, joint);
    }

    public static void requestRemoveJoint(ConstraintKey key){
        getOrCreate(key.dimension()).requestRemove(key);
    }

    public static void destroyAllJoints(ServerLevel level, BlockPos pos){
        VSGetterUtils.getShip(level, pos).map(Ship::getId).ifPresent(id -> {
            getOrCreate(ValkyrienSkies.getDimensionId(level)).destroyRequests.offer(id);
        });
    }

    private final Queue<request> requests = Queues.newConcurrentLinkedQueue();
    private final Queue<Long> destroyRequests = Queues.newConcurrentLinkedQueue();
    // private int lazyTickCounter = 0;
    private final Executor deferralJointAdder = new Executor();
    private boolean removeInvalidCheck = false;
    private final String dimensionId;

    public JointHandler(String dimensionId) {
        this.dimensionId = dimensionId;
    }

    public void physTick(PhysLevel world, double delta){
        if(!world.getDimension().equals(dimensionId)){
            ControlCraft.LOGGER.warn("JointHandler is not in the correct dimension: {} != {}", world.getDimension(), dimensionId);
            return;
        }

        PhysLevelCore core = (PhysLevelCore) world;
        while (!requests.isEmpty()){
            request req = Objects.requireNonNull(requests.poll());

            if(req.op == Operation.CREATE_OR_UPDATE){
                createOrReplaceNewJoint(core, req.key, req.joint);
            } else if(req.op == Operation.REMOVE){
                removeJoint(core, req.key);
            }
        }
        while (!destroyRequests.isEmpty()){
            long id = Objects.requireNonNull(destroyRequests.poll());
            core.getJointsFromShip(id).forEach(joint -> {
                if(joint != null){
                    core.removeJoint(joint);
                }
            });
        }
        deferralJointAdder.tick();

        removeIfNeeded(core);
    }

    public void removeIfNeeded(PhysLevelCore world){
        if(removeInvalidCheck){
            removeInvalidCheck = false;
            removeInvalid(world);
        }
    }

    public void removeInvalid(PhysLevelCore world){
        cache
            .entrySet()
            .stream()
            .filter(e -> world.getJointById(e.getValue().ID()) == null).toList()
            .forEach(e -> {
                ControlCraft.LOGGER.warn("Joint {} is invalid, removing it", e.getKey());
                cache.remove(e.getKey());
            });
    }

    private static GameToPhysicsAdapter adapter(String dimensionId){
        return ValkyrienSkiesMod.getOrCreateGTPA(dimensionId);
    }

    private void createOrReplaceNewJoint(PhysLevelCore world, ConstraintKey key, VSJoint joint){
        if(cache.containsKey(key) && world.getJointById(cache.get(key).ID()) != null){
            ConstraintWithID exist = cache.get(key);
            // adapter(key.dimension()).updateJoint(new VSJointAndId(exist.ID(), joint));
            world.updateJoint(exist.ID(), joint);
            cache.put(key, new ConstraintWithID(joint, exist.ID()));
        }else {
            cache.remove(key);
//            adapter(key.dimension()).addJoint(joint, 4, id -> {
//                cache.put(key, new ConstraintWithID(joint, id));
//            });
//            ValkyrienSkiesMod.getOrCreateGTPA(ValkyrienSkies.getDimensionId(ControlCraftServer.OVERWORLD)).addJoint(
//                    joint,
//                    4,
//                    id -> {
//                        cache.put(key, new ConstraintWithID(joint, id));
//                    }
//            );
            deferralJointAdder.executeLater(() -> {
                int id = world.addJoint(joint);
                cache.put(key, new ConstraintWithID(joint, id));
            },
                    4
            );

//            int id = world.addJoint(joint);
//            cache.put(key, new ConstraintWithID(joint, id));
        }
    }

    private void removeJoint(PhysLevelCore world, ConstraintKey key){
        if(cache.containsKey(key)){
            ConstraintWithID exist = cache.get(key);
            // world.removeJoint(exist.ID());
            adapter(key.dimension()).removeJoint(exist.ID());
            cache.remove(key);
        }
    }

//    public @Nullable VSJoint retrieve(ConstraintKey key){
//        return cache.containsKey(key) ? cache.get(key).constrain() : null;
//    }

    public void requestOverride(ConstraintKey key, VSJoint joint){
        requests.offer(new request(key, joint, Operation.CREATE_OR_UPDATE));
    }

    public void requestRemove(ConstraintKey key){
        requests.offer(new request(key, null, Operation.REMOVE));
    }

    enum Operation {
        CREATE_OR_UPDATE,
        REMOVE,
    }


    record request(ConstraintKey key, VSJoint joint, Operation op){}
}

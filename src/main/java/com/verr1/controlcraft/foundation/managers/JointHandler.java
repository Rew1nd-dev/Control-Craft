package com.verr1.controlcraft.foundation.managers;

import com.google.common.collect.Maps;
import com.google.common.collect.Queues;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.constraint.ConstraintKey;
import com.verr1.controlcraft.foundation.data.constraint.ConstraintWithID;
import com.verr1.controlcraft.foundation.data.constraint.SavedConstraintObject;
import com.verr1.controlcraft.utils.VSGetterUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.api.world.PhysLevel;
import org.valkyrienskies.core.apigame.joints.VSJoint;
import org.valkyrienskies.core.apigame.world.PhysLevelCore;
import org.valkyrienskies.mod.api.ValkyrienSkies;

import java.util.*;


public class JointHandler {


    private final Map<ConstraintKey, ConstraintWithID> cache = Maps.newConcurrentMap();

    private final Queue<request> requests = Queues.newConcurrentLinkedQueue();
    private final Queue<Long> destroyRequests = Queues.newConcurrentLinkedQueue();

    public void physTick(PhysLevel world, double delta){
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
    }

    public void onServerStaring(MinecraftServer server){
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

    public void onServerStopping(MinecraftServer $){
        saveAll();
    }

    public void saveAll(){
        ConstraintSavedData storage = ConstraintSavedData.load(ControlCraftServer.INSTANCE);
        storage.clear();
        ControlCraft.LOGGER.info("Saving {} constrains", cache.size());
        cache.forEach((key, data) -> {
            try{
                storage.put(key, data.constrain());
            }catch (Exception e){
                ControlCraft.LOGGER.error("Failed to save constrain", e);
            }
        });
    }

    public void destroyAllJoints(ServerLevel level, BlockPos pos){
        VSGetterUtils.getShip(level, pos).map(Ship::getId).ifPresent(destroyRequests::offer);
    }

    private void createOrReplaceNewJoint(PhysLevelCore world, ConstraintKey key, VSJoint joint){
        if(cache.containsKey(key)){
            // Replace the existing joint
            ConstraintWithID exist = cache.get(key);
            world.updateJoint(exist.ID(), joint);
            cache.put(key, new ConstraintWithID(joint, exist.ID()));
        }else {
            // Create a new joint
            int id = world.addJoint(joint);
            cache.put(key, new ConstraintWithID(joint, id));
        }
    }

    private void removeJoint(PhysLevelCore world, ConstraintKey key){
        if(cache.containsKey(key)){
            ConstraintWithID exist = cache.get(key);
            world.removeJoint(exist.ID());
            cache.remove(key);
        }
    }

    public @Nullable VSJoint retrieveJoint(ConstraintKey key){
        return cache.containsKey(key) ? cache.get(key).constrain() : null;
    }

    public void requestOverrideJoint(ConstraintKey key, VSJoint joint){
        requests.offer(new request(key, joint, Operation.CREATE_OR_UPDATE));
    }

    public void requestRemoveJoint(ConstraintKey key){
        requests.offer(new request(key, null, Operation.REMOVE));
    }

    enum Operation {
        CREATE_OR_UPDATE,
        REMOVE,
    }

    record request(ConstraintKey key, VSJoint joint, Operation op){}
}

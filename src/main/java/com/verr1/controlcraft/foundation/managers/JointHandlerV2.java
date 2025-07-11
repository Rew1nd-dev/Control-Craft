package com.verr1.controlcraft.foundation.managers;

import com.google.common.collect.Maps;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.constraint.ConstraintKey;
import com.verr1.controlcraft.foundation.data.constraint.ConstraintWithID;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.Nullable;
import org.valkyrienskies.core.apigame.joints.VSJoint;
import org.valkyrienskies.core.apigame.world.PhysLevelCore;
import org.valkyrienskies.mod.api.ValkyrienSkies;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.ValkyrienSkiesMod;
import org.valkyrienskies.mod.common.util.GameToPhysicsAdapter;

import java.util.Map;

public class JointHandlerV2 {

    private final Map<ConstraintKey, ConstraintWithID> cache = Maps.newConcurrentMap();




    public @Nullable VSJoint retrieveJoint(ConstraintKey key){
        return cache.containsKey(key) ? cache.get(key).constrain() : null;
    }


    private static GameToPhysicsAdapter adapter(String dimensionId){
        return ValkyrienSkiesMod.getOrCreateGTPA(dimensionId);
    }

    public void requestOverrideJoint(ConstraintKey key, VSJoint joint){

    }

    public void requestRemoveJoint(ConstraintKey key){

    }


}

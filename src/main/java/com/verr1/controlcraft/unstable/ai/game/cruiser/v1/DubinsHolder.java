package com.verr1.controlcraft.unstable.ai.game.cruiser.v1;

import com.verr1.controlcraft.unstable.pathing.LerpPath;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3dc;

import java.util.ArrayList;
import java.util.List;

public class DubinsHolder {
    public static final Serializer<List<Vector3dc>> SER = SerializeUtils.ofList(SerializeUtils.VECTOR3DC);
    public static final Serializer<DubinsHolder> SERIALIZER = SerializeUtils.of(
            DubinsHolder::serialize,
            DubinsHolder::deserialize
    );

    private final List<Vector3dc> wayPointsQueue = new ArrayList<>();

    public DubinsHolder(LerpPath<Vector3dc> lerpPath){
        reset(lerpPath);
    }

    public List<Vector3dc> asList(){
        return wayPointsQueue.stream().toList();
    }

    public DubinsHolder(){}

    public boolean finished(){
        return wayPointsQueue.isEmpty();
    }



    public @Nullable Vector3dc finalGoal(){
        if(wayPointsQueue.isEmpty())return null;
        return wayPointsQueue.get(wayPointsQueue.size() - 1);
    }

    public List<Vector3dc> all(){
        return wayPointsQueue;
    }

    public void arrive(){
        wayPointsQueue.remove(0);
    }

    public void reset(LerpPath<Vector3dc> lerpPath){
        wayPointsQueue.clear();
        if(lerpPath == null)return;
        for (int i = 0; i < lerpPath.segments(); i++){
            wayPointsQueue.add(lerpPath.lerp(i));
        }
    }

    public CompoundTag serialize(){
        return SER.serialize(asList());
    }

    public static DubinsHolder deserialize(CompoundTag tag){
        List<Vector3dc> points = SER.deserialize(tag);
        DubinsHolder holder = new DubinsHolder();
        holder.wayPointsQueue.addAll(points);
        return holder;
    }


}

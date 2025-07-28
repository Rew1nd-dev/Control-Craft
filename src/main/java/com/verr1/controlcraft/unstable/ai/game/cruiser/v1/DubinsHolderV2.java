package com.verr1.controlcraft.unstable.ai.game.cruiser.v1;

import com.verr1.controlcraft.unstable.pathing.LerpPathV2;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.nbt.CompoundTag;
import org.joml.Vector3dc;

import java.util.ArrayList;
import java.util.List;

public class DubinsHolderV2 implements LerpPathV2<Vector3dc>{
    public static final Serializer<List<Vector3dc>> SER = SerializeUtils.ofList(SerializeUtils.VECTOR3DC);
    public static final Serializer<DubinsHolderV2> SERIALIZER = SerializeUtils.of(
            DubinsHolderV2::serialize,
            DubinsHolderV2::deserialize
    );

    private final LerpPathV2<Vector3dc> lerpPath;
    private final Vector3dc goal;


    public DubinsHolderV2(LerpPathV2<Vector3dc> lerpPath){
        this.lerpPath = lerpPath;
        goal = lerpPath.point(lerpPath.length());
    }

    public Vector3dc goal(){
        return goal;
    }

    public List<Vector3dc> asList(){
        int sample = 30;
        List<Vector3dc> points = new ArrayList<>();
        for (int i = 0; i < sample; i++) {
            double length = (double) i / (sample - 1) * lerpPath.length();
            points.add(lerpPath.point(length));
        }
        return points;
    }



    public CompoundTag serialize(){
        return SER.serialize(asList());
    }

    public static DubinsHolderV2 deserialize(CompoundTag tag){
        List<Vector3dc> points = SER.deserialize(tag);
        return new DubinsHolderV2(LerpPathV2.fromList(points));
    }


    @Override
    public double length() {
        return lerpPath.length();
    }

    @Override
    public Vector3dc point(double length) {
        return lerpPath.point(length);
    }
}

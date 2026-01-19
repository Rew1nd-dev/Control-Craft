package com.verr1.controlcraft.unstable.management.v1;

import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.nbt.CompoundTag;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBic;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public interface AIYardAllocator {

    Vector3dc position(Long pointer);

    Long allocate(AABBic bounds);

    void free(Long pointer);

    void clear();

    default CompoundTag serialize(){
        return new CompoundTag();
    }

    default void deserialize(CompoundTag tag){}

    class Simple implements AIYardAllocator{

        public final Supplier<Vector3dc> position;

        private final Map<Long, Double> pointerToDistance = new HashMap<>();

        public Simple(Supplier<Vector3dc> position) {
            this.position = position;
        }



        @Override
        public Vector3dc position(Long pointer) {
            return position.get().fma(distance(pointer), AIControlUtils.randUnit3d(), new Vector3d());
        }

        private double distance(long pointer){
            return pointerToDistance.getOrDefault(pointer, 10.0);
        }

        @Override
        public Long allocate(AABBic bounds) {
            double d = Math.pow(MathUtils.volume(bounds), 1.0/3.0) + 5;
            long p = firstFreePointer();
            pointerToDistance.put(p, d);
            return p;
        }

        private long firstFreePointer(){
            long p = 1L;
            while(pointerToDistance.containsKey(p))p++;
            return p;
        }

        @Override
        public void free(Long pointer) {
            pointerToDistance.remove(pointer);
        }


        @Override
        public void clear() {
            pointerToDistance.clear();
        }
    }

}

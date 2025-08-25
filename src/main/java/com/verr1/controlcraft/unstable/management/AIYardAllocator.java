package com.verr1.controlcraft.unstable.management;

import com.verr1.controlcraft.unstable.valkyrienskies.controls.AIControlUtils;
import net.minecraft.nbt.CompoundTag;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.joml.primitives.AABBi;
import org.joml.primitives.AABBic;

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

        public final Vector3dc position;

        public Simple(Vector3dc position) {
            this.position = position;
        }

        @Override
        public Vector3dc position(Long pointer) {
            return position.fma(10, AIControlUtils.randUnit3d(), new Vector3d());
        }

        @Override
        public Long allocate(AABBic bounds) {
            return 0L;
        }

        @Override
        public void free(Long pointer) {

        }


        @Override
        public void clear() {}
    }

}

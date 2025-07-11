package com.verr1.controlcraft.foundation.data.constraint;

import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import org.valkyrienskies.core.apigame.joints.VSD6Joint;

import javax.annotation.Nullable;
import java.util.Objects;

public record AngleLimit(double low, double high) {
    private static final double MAGIC_NUMBER = 0;
    public static final AngleLimit FREE = new AngleLimit(MAGIC_NUMBER, MAGIC_NUMBER);

    public @Nullable VSD6Joint.AngularLimitPair toPhysX(Direction dir){
        if(Math.abs(low - MAGIC_NUMBER) + Math.abs(high - MAGIC_NUMBER ) < 1e-6)return null;
        return new VSD6Joint.AngularLimitPair(
                (float) MathUtils.angleReset(low + AngleFix(dir)),
                (float) MathUtils.angleReset(high + AngleFix(dir)),
                null, null, null, null
        );
    }

    public static double AngleFix(Direction dir){
        return switch (dir){
            case DOWN -> 0.0;
            case UP -> -Math.PI;
            case NORTH -> 0.0;
            case SOUTH -> -Math.PI;
            case WEST -> 0.0;
            case EAST -> -Math.PI;
        };
    }

    // from, to are (-PI, PI)
    public static AngleLimit fromTo(double from, double to, boolean clockwise){
        if(Math.abs(from - to) < 1e-6){
            return FREE;
        }


        double low = Math.min(from, to);
        double high = Math.max(from, to);

        return clockwise ?
                new AngleLimit(low, high) :
                new AngleLimit(high, low + 2 * Math.PI);
    }

    public CompoundTag serialize(){
        CompoundTag tag = new CompoundTag();
        tag.putDouble("low", low);
        tag.putDouble("high", high);
        return tag;
    }

    public static AngleLimit deserialize(CompoundTag tag) {
        double low = tag.getDouble("low");
        double high = tag.getDouble("high");
        return new AngleLimit(low, high);
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (!(object instanceof AngleLimit that)) return false;
        return Math.abs(low - that.low) < 1e-6 && Math.abs(high - that.high) < 1e-6;
    }

    @Override
    public int hashCode() {
        return Objects.hash(low, high);
    }
}

package com.verr1.controlcraft.foundation.data.links;

import com.verr1.controlcraft.foundation.data.render.CimulinkWireEntry;
import com.verr1.controlcraft.foundation.data.render.RenderableOutline;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3dc;

import java.util.Objects;

import static org.valkyrienskies.mod.api.ValkyrienSkies.toJOML;


public record RenderCurveKey(
        BlockPort in,
        BlockPort out,
        // These 2 identify what are exactly connected
        // These below helps with rendering the curve
        Vec3 inVec,
        Vec3 outVec,
        Direction inDir,
        Direction outDir,
        double startOffset,
        double endOffset
) {

    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (!(object instanceof RenderCurveKey that)) return false;
        return Vec3Equals(inVec, that.inVec)
            && Vec3Equals(outVec, that.outVec)
            && Objects.equals(in, that.in) && Objects.equals(out, that.out)
            && inDir == that.inDir
            && outDir == that.outDir;
    }

    public static boolean Vec3Equals(Vec3 x, Vec3 y){
        return x.distanceToSqr(y) < 1e-8;
    }

    @Override
    public int hashCode() {
        return Objects.hash(in, out);
    }

    public RenderableOutline createBezier(){
        Vector3dc inJoml = toJOML(Vec3.atLowerCornerOf(inDir.getNormal()));
        Vector3dc outJoml = toJOML(Vec3.atLowerCornerOf(outDir.getNormal()));
        return new CimulinkWireEntry(
                toJOML(outVec).fma(-startOffset, outJoml),
                toJOML(inVec).fma(-endOffset, inJoml),
                outJoml,
                inJoml,
                0.067f,
                20
        );
    }

}

package com.verr1.controlcraft.ponder.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.verr1.controlcraft.foundation.data.render.CimulinkWireEntry;
import com.verr1.controlcraft.foundation.data.render.RenderableOutline;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.Vec3;
import org.joml.Matrix4d;
import org.joml.Matrix4dc;
import org.joml.Vector3dc;

public class CimulinkPonderWireEntry extends CimulinkWireEntry {


    public CimulinkPonderWireEntry(
            Vector3dc start,
            Vector3dc end,
            Vector3dc startDirection,
            Vector3dc endDirection
    ) {
        super(start, end, startDirection, endDirection, 0.067f, 20);
    }

    @Override
    public Matrix4dc transformAt(Vector3dc v) {
        // Ponder wire does not need to transform with ships
        return new Matrix4d();
    }

    @Override
    public void checkAlways() {
        // Ponder wire won't change
    }

}

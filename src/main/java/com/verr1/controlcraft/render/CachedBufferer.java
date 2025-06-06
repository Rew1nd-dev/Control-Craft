package com.verr1.controlcraft.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.verr1.controlcraft.ControlCraftClient;

import dev.engine_room.flywheel.lib.model.baked.PartialModel;
import dev.engine_room.flywheel.lib.transform.TransformStack;
import net.createmod.catnip.math.AngleHelper;
import net.createmod.catnip.render.SuperBufferFactory;
import net.createmod.catnip.render.SuperByteBuffer;
import net.createmod.catnip.render.SuperByteBufferCache;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.state.BlockState;
import org.apache.commons.lang3.tuple.Pair;

import java.util.function.Supplier;

import static net.minecraft.world.level.block.state.properties.BlockStateProperties.FACING;

public class CachedBufferer {
    public static final SuperByteBufferCache.Compartment<BlockState> CC_GENERIC_BLOCK = new SuperByteBufferCache.Compartment<>();
    public static final SuperByteBufferCache.Compartment<PartialModel> CC_PARTIAL = new SuperByteBufferCache.Compartment<>();
    public static final SuperByteBufferCache.Compartment<Pair<Direction, PartialModel>> CC_DIRECTIONAL_PARTIAL = new SuperByteBufferCache.Compartment<>();

    public static SuperByteBuffer block(BlockState toRender) {
        return block(CC_GENERIC_BLOCK, toRender);
    }

    public static SuperByteBuffer block(SuperByteBufferCache.Compartment<BlockState> compartment, BlockState toRender) {
        return ControlCraftClient.BUFFER_CACHE.get(compartment, toRender, () -> SuperBufferFactory.getInstance().createForBlock(toRender));
    }

    public static SuperByteBuffer partial(PartialModel partial, BlockState referenceState) {
        return ControlCraftClient.BUFFER_CACHE.get(CC_PARTIAL, partial,
                () -> SuperBufferFactory.getInstance().createForBlock(partial.get(), referenceState));
    }

    public static SuperByteBuffer partial(PartialModel partial, BlockState referenceState,
                                          Supplier<PoseStack> modelTransform) {
        return ControlCraftClient.BUFFER_CACHE.get(CC_PARTIAL, partial,
                () -> SuperBufferFactory.getInstance().createForBlock(partial.get(), referenceState, modelTransform.get()));
    }

    public static SuperByteBuffer partialFacing(PartialModel partial, BlockState referenceState) {
        Direction facing = referenceState.getValue(FACING);
        return partialFacing(partial, referenceState, facing);
    }

    public static SuperByteBuffer partialFacing(PartialModel partial, BlockState referenceState, Direction facing) {
        return partialDirectional(partial, referenceState, facing,
                rotateToFace(facing));
    }

    public static SuperByteBuffer partialFacingVertical(PartialModel partial, BlockState referenceState, Direction facing) {
        return partialDirectional(partial, referenceState, facing,
                rotateToFaceVertical(facing));
    }

    public static SuperByteBuffer partialDirectional(PartialModel partial, BlockState referenceState, Direction dir,
                                                     Supplier<PoseStack> modelTransform) {
        return ControlCraftClient.BUFFER_CACHE.get(CC_DIRECTIONAL_PARTIAL, Pair.of(dir, partial),
                () -> SuperBufferFactory.getInstance().createForBlock(partial.get(), referenceState, modelTransform.get()));
    }

    public static Supplier<PoseStack> rotateToFace(Direction facing) {
        return () -> {
            PoseStack stack = new PoseStack();
            TransformStack.of(stack)
                    .center()
                    .rotateY(AngleHelper.horizontalAngle(facing))
                    .rotateX(AngleHelper.verticalAngle(facing))
                    .uncenter();
            return stack;
        };
    }

    public static Supplier<PoseStack> rotateToFaceVertical(Direction facing) {
        return () -> {
            PoseStack stack = new PoseStack();
            TransformStack.of(stack)
                    .center()
                    .rotateY(AngleHelper.horizontalAngle(facing))
                    .rotateX(AngleHelper.verticalAngle(facing) + 90)
                    .uncenter();
            return stack;
        };
    }
}

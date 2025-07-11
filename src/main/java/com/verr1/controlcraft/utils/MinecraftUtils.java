package com.verr1.controlcraft.utils;

import com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.FlyingMob;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LightLayer;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;

public class MinecraftUtils {
    public static void updateBlockState(@Nullable Level world, BlockPos pos, BlockState newState){
        Optional.ofNullable(world).ifPresent(w -> w.setBlock(pos, newState, 3));
    }

    @OnlyIn(Dist.CLIENT)
    public static @Nullable Direction lookingAtFaceDirection(){
        return Optional
                .ofNullable(Minecraft.getInstance().player)
                .map(player -> player.pick(5, Minecraft.getInstance().getPartialTick(), false))
                .filter(hitResult -> hitResult.getType() == HitResult.Type.BLOCK)
                .map(hitResult -> (BlockHitResult) hitResult)
                .map(BlockHitResult::getDirection)
                .orElse(null);
    }

    @OnlyIn(Dist.CLIENT)
    public static int getPerceivedLightLevel(BlockPos pos) {
        Minecraft mc = Minecraft.getInstance();
        Level level = mc.level;
        if (level == null) {
            return 0; // 如果世界未加载，返回0或抛出异常
        }

        if (level.canSeeSky(pos)) {
            // 方块直接暴露在天空下，返回天空光照
            return level.getBrightness(LightLayer.SKY, pos);
        } else {
            // 方块有遮挡，返回综合光照
            return level.getRawBrightness(pos, 0);
        }
    }

    @OnlyIn(Dist.CLIENT)
    public static @Nullable BlockEntity lookingAt(){
        Minecraft mc = Minecraft.getInstance();
        return Optional
                .ofNullable(mc.player)
                .map(player -> player.pick(5, mc.getPartialTick(), false))
                .filter(BlockHitResult.class::isInstance)
                .map(BlockHitResult.class::cast)
                .map(BlockHitResult::getBlockPos)
                .flatMap(
                    p -> Optional
                            .ofNullable(mc.level)
                            .map(level -> level.getBlockEntity(p)))
                .orElse(null);
    }

    @OnlyIn(Dist.CLIENT)
    public static @Nullable BlockPos lookingAtPos(){
        Minecraft mc = Minecraft.getInstance();
        return Optional
                .ofNullable(mc.hitResult)
                .filter(BlockHitResult.class::isInstance)
                .map(BlockHitResult.class::cast)
                .map(BlockHitResult::getBlockPos)
                .orElse(null);
    }

    @OnlyIn(Dist.CLIENT)
    public static @Nullable Vec3 lookingAtVec(){
        Minecraft mc = Minecraft.getInstance();
        return Optional
                .ofNullable(mc.hitResult)
                .filter(BlockHitResult.class::isInstance)
                .map(BlockHitResult.class::cast)
                .map(BlockHitResult::getLocation)
                .orElse(null);
    }

    @OnlyIn(Dist.CLIENT)
    public static<T extends Descriptive<?>> int maxLength(List<T> descriptive){
        AtomicInteger maxLen = new AtomicInteger(0);
        descriptive.forEach(c -> {
            int len = Minecraft.getInstance().font.width(c.asComponent().copy().withStyle(Converter::optionStyle));
            if(len > maxLen.get()) maxLen.set(len);
        });
        return maxLen.get();
    }

    @OnlyIn(Dist.CLIENT)
    public static<T extends Descriptive<?>> int maxLength(T... descriptive){
        return maxLength(Arrays.asList(descriptive));
    }

    @OnlyIn(Dist.CLIENT)
    public static<T extends Descriptive<?>> int maxLength(UnaryOperator<Style> style, List<T> descriptive){
        AtomicInteger maxLen = new AtomicInteger(0);
        descriptive.forEach(c -> {
            int len = Minecraft.getInstance().font.width(c.asComponent().copy().withStyle(style));
            if(len > maxLen.get()) maxLen.set(len);
        });
        return maxLen.get();
    }

    @OnlyIn(Dist.CLIENT)
    public static<T extends Descriptive<?>> int maxLength(UnaryOperator<Style> style, T... descriptive){
        return maxLength(style, Arrays.asList(descriptive));
    }

    @OnlyIn(Dist.CLIENT)
    public static int maxTitleLength(List<String> descriptive){
        AtomicInteger maxLen = new AtomicInteger(0);
        descriptive.forEach(c -> {
            int len = Minecraft.getInstance().font.width(Component.literal(c).withStyle(Converter::titleStyle));
            if(len > maxLen.get()) maxLen.set(len);
        });
        return maxLen.get();
    }

    @OnlyIn(Dist.CLIENT)
    public static  <T> Optional<T> getBlockEntityAt(@NotNull BlockPos pos, Class<T> clazz){
        return Optional
                .ofNullable(Minecraft.getInstance().level)
                .map(world -> world.getExistingBlockEntity(pos))
                .filter(clazz::isInstance)
                .map(clazz::cast);
    }

    public static Direction getVerticalDirectionSimple(Direction facing){
        if(facing.getAxis() != Direction.Axis.Y)return Direction.UP;
        return Direction.NORTH;
    }

    public static Vec3 toVec3(Vec3i vec3i){
        return new Vec3(vec3i.getX(), vec3i.getY(), vec3i.getZ());
    }

    public static Direction getVerticalDirection(BlockState state){
        if(!state.hasProperty(BlockStateProperties.FACING) ||
                !state.hasProperty(DirectionalAxisKineticBlock.AXIS_ALONG_FIRST_COORDINATE))return Direction.UP;

        Direction facing = state.getValue(BlockStateProperties.FACING);
        Boolean align = state.getValue(DirectionalAxisKineticBlock.AXIS_ALONG_FIRST_COORDINATE);
        if(facing.getAxis() != Direction.Axis.X){
            if(align)return Direction.EAST;
            return facing.getAxis() == Direction.Axis.Y ? Direction.SOUTH : Direction.UP;
        }
        if(align)return Direction.UP;
        return Direction.SOUTH;
    }


    public static List<Entity> getLivingEntities(Vec3 center, double radius, @NotNull Level level){
        return level.getEntities(
                (Entity) null,
                new AABB(
                        center.x - radius, center.y - radius, center.z - radius,
                        center.x + radius, center.y + radius, center.z + radius),
                LivingEntity.class::isInstance
        );
    }

    public static List<Entity> getMobs(Vec3 center, double radius, @NotNull Level level){
        return level.getEntities(
                (Entity) null,
                new AABB(
                        center.x - radius, center.y - radius, center.z - radius,
                        center.x + radius, center.y + radius, center.z + radius),
                entity -> entity instanceof Monster || entity instanceof FlyingMob
        );
    }




}

package com.verr1.controlcraft.utils;

import com.simibubi.create.content.kinetics.base.DirectionalAxisKineticBlock;
import com.verr1.controlcraft.content.gui.factory.Converter;
import com.verr1.controlcraft.content.gui.layouts.api.Descriptive;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.core.particles.ParticleOptions;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.Style;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundEvent;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.FlyingMob;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.ChunkPos;
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
import net.minecraftforge.server.ServerLifecycleHooks;
import net.shao.valkyrien_space_war.particle.explotion.ExplosionSmokeOptions;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;

import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.UnaryOperator;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

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
    public static void sendClientMessage(String mst){
        Player p = Minecraft.getInstance().player;
        if(p == null)return;
        p.sendSystemMessage(Component.literal(mst));
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

    public static boolean _isChunkInRange(int x1, int z1, int x2, int z2, int viewDistance){
        return new ChunkPos(x1, z1).getChessboardDistance(new ChunkPos(x2, z2)) <= viewDistance + 1;
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

    public static void broadcastMessage(Component message){
        MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
        if(server == null){
            return;
        };
        server.getPlayerList().getPlayers().forEach(p -> p.sendSystemMessage(message));
    }

    public static void broadcastMessage(Component message, Vec3 position, double radius){
        MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
        if(server == null){
            return;
        };
        server.getPlayerList().getPlayers()
                .stream()
                .filter(p -> p.position().distanceTo(position) < radius)
                .forEach(p -> p.sendSystemMessage(message));
    }

    public static void spawnParticleAt(Vec3 position, ParticleOptions opt){
        MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
        if(server == null){
            return;
        };
        server.getAllLevels().forEach(lvl -> {
            lvl.sendParticles(opt, position.x, position.y, position.z, 1, 0.0, 0.0, 0.0, 0.0);
        });
    }

    public static void playSoundAt(Vec3 position, SoundEvent soundEvent, float volume, float pitch){
        MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
        if(server == null){
            return;
        };
        server.getAllLevels().forEach(lvl -> {
            lvl.playSound(null, position.x, position.y, position.z, soundEvent, SoundSource.PLAYERS, volume, pitch);
        });
    }

    public static void broadcastMessageAsBook(List<Component> lines){
        ItemStack book = new ItemStack(Items.WRITTEN_BOOK);

        CompoundTag tag = new CompoundTag();

        // 设置书的标题和作者
        tag.putString("title", "自定义书籍");
        tag.putString("author", "你的名字");

        // 创建页面列表
        ListTag pages = new ListTag();

        int maxLinesPerPage = 14;

        for (var i = 0; i < lines.size(); i += maxLinesPerPage) {
            int end = Math.min(i + maxLinesPerPage, lines.size());
            List<Component> pageLines = lines.subList(i, end);
            StringBuilder pageContent = new StringBuilder();
            for (Component line : pageLines) {
                pageContent.append(line.getString()).append("\n");
            }
            // 移除最后一个换行符
            if (!pageContent.isEmpty()) {
                pageContent.setLength(pageContent.length() - 1);
            }
            String pageJson = "{\"text\":\"" + pageContent.toString().replace("\"", "\\\"") + "\"}";
            pages.add(StringTag.valueOf(pageJson));
        }


        // 添加页面内容（使用 JSON 文本格式）
//        String page1 = "{\"text\":\"这是第一页内容。\\n换行示例。\"}";
//        String page2 = "{\"text\":\"第二页内容。\\n\\n支持多行文本。\"}";
//        String page3 = "{\"text\":\"第三页\\n特殊格式：\",\"extra\":[{\"text\":\"粗体\",\"bold\":true},{\"text\":\" 斜体\",\"italic\":true},{\"text\":\" 颜色\",\"color\":\"red\"}]}";
//
//        pages.add(StringTag.valueOf(page1));
//        pages.add(StringTag.valueOf(page2));
//        pages.add(StringTag.valueOf(page3));

        // 将页面添加到 NBT
        tag.put("pages", pages);

        // 标记为已解析（对于已写好的书）
        tag.putBoolean("resolved", true);

        // 将 NBT 应用到书本
        book.setTag(tag);

    }

    public static void giveItemToAll(ItemStack stack){
        getAllPlayers().forEach(p -> p.drop(stack.copy(), false));
    }

    public static List<ServerPlayer> getAllPlayers(){
        MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
        if(server == null){
            return List.of();
        };
        return server.getPlayerList().getPlayers();
    }

    public static void broadcastMessage(String message){
        broadcastMessage(Component.literal(message));
    }

    public static void broadcastMessage(String message, Vec3 position, double radius){
        broadcastMessage(Component.literal(message), position, radius);
    }

}

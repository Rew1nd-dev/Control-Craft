package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.content.links.integration.CircuitBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import net.minecraft.network.chat.Component;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraftforge.fml.loading.FMLPaths;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class CircuitCompilerItem extends Item {

    public static final Path CIMULINKS = FMLPaths.GAMEDIR.get().resolve("cimulinks");

    public CircuitCompilerItem(Properties p_41383_) {
        super(p_41383_);
    }

    @Override
    public InteractionResult useOn(UseOnContext context) {
        Level world = context.getLevel();
        if (!world.isClientSide) { // 确保在服务端执行
            ItemStack stack = context.getItemInHand();
            Player player = context.getPlayer();
            BlockPos pos = context.getClickedPos();

            if(player == null)return InteractionResult.FAIL;

            CompoundTag nbt = stack.getOrCreateTag();
            // 检查NBT数据是否存在
            if (nbt.contains("circuitNbt")) {
                // 这里可以添加放置电路的逻辑
                CompoundTag circuitNbt = nbt.getCompound("circuitNbt");
                CircuitNbt nbtHolder = CircuitNbt.deserialize(circuitNbt);

                BlockEntityGetter.getLevelBlockEntityAt(world, pos, CircuitBlockEntity.class)
                        .ifPresentOrElse(
                                cbe -> {
                                    try {
                                        cbe.loadCircuit(nbtHolder);
                                    }catch (IllegalArgumentException e){
                                        player.sendSystemMessage(Component.literal("Failed to load circuit: " + e.getMessage()));
                                    }
                                },
                                () -> player.sendSystemMessage(Component.literal("Not a circuit block found at the selected position."))
                        );

            }
        }
        return InteractionResult.PASS;
    }


    public static void save(String saveName, ItemStack stack){
        Path file = CIMULINKS.resolve(saveName + ".nbt").toAbsolutePath();
        CompoundTag data = stack.getOrCreateTag();
        try{
            Files.createDirectories(CIMULINKS);
            try(OutputStream out = Files.newOutputStream(file, StandardOpenOption.CREATE)){
                NbtIo.writeCompressed(data, out);
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void load(String loader, String saveName, ItemStack stack) throws IOException{
        Path file = CIMULINKS.resolve(saveName + ".nbt").toAbsolutePath();
        if(!Files.exists(file)){
            file = CIMULINKS.resolve(loader).resolve(saveName + ".nbt").toAbsolutePath();
        }
        InputStream in = Files.newInputStream(file, StandardOpenOption.CREATE);
        CompoundTag tag = NbtIo.readCompressed(in);
        stack.setTag(tag);
    }

    public static CompoundTag loadTag(String saveName){
        Path file = CIMULINKS.resolve(saveName + ".nbt").toAbsolutePath();

        try(InputStream in = Files.newInputStream(file, StandardOpenOption.CREATE)){
            return NbtIo.readCompressed(in);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void saveTag(CompoundTag uploaded, String saveName, String uploader){
        Path file = CIMULINKS.resolve(uploader).resolve(saveName + ".nbt").toAbsolutePath();

        try{
            Files.createDirectories(CIMULINKS);
            try(OutputStream out = Files.newOutputStream(file, StandardOpenOption.CREATE)){
                NbtIo.writeCompressed(uploaded, out);
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


}

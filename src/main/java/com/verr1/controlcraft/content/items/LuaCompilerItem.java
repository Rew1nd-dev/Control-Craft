package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.content.links.integration.CircuitBlockEntity;
import com.verr1.controlcraft.content.links.integration.LuaBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
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
import org.luaj.vm2.LuaError;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

import static com.verr1.controlcraft.content.items.CircuitCompilerItem.CIMULINKS;

public class LuaCompilerItem extends Item {

    public static final Path LUALINKS = FMLPaths.GAMEDIR.get().resolve("lualinks");

    public LuaCompilerItem(Properties property) {
        super(property.stacksTo(1));
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
            if (nbt.contains("luaNbt")) {
                // 这里可以添加放置电路的逻辑
                CompoundTag circuitNbt = nbt.getCompound("luaNbt");
                LuacuitScript nbtHolder = LuacuitScript.deserialize(circuitNbt);

                BlockEntityGetter.getLevelBlockEntityAt(world, pos, LuaBlockEntity.class)
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


    public static void load(String saveName, ItemStack stack){
        Path file = LUALINKS.resolve(saveName + ".lua").toAbsolutePath();

        try{
            String code = loadLua(file.toString());

            LuacuitScript ls = LuacuitScript.fromCode(code);

            stack.getOrCreateTag().put("luaNbt", ls.serialize());

        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (LuaOvertimeException | LuaError e){
            throw new IllegalArgumentException("Failed to compile Lua script: " + e.getMessage());
        }
    }

    public static String loadLua(String path) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = br.readLine()) != null) {
                content.append(line).append("\n");
            }
            return content.toString();
        } catch (IOException e) {
            throw e;
        }
    }

}

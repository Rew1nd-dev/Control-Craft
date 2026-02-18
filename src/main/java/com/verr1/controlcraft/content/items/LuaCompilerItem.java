package com.verr1.controlcraft.content.items;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.links.integration.LuaBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.LuaOvertimeException;
import com.verr1.controlcraft.foundation.data.links.IntegrationPortStatus;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
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
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Objects;

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
                                        cbe.linkPort().setValuesOnly(
                                            nbtHolder
                                                .definedInputs().stream()
                                                .map(
                                                    in -> new IntegrationPortStatus(in, nbtHolder.getDefault(in), true, false)
                                                )
                                                .toList()
                                        );
                                        cbe.setDeviceName(stack.getHoverName().getString());
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


    public static void load(String loader, String saveName, ItemStack stack) throws IOException{
        Path file = LUALINKS.resolve(saveName + ".lua").toAbsolutePath();

        if(!Files.exists(file)){
            file = LUALINKS.resolve(loader).resolve(saveName + ".lua").toAbsolutePath();
        }

        try{
            String code = loadLua(file.toString());
            LuacuitScript ls = LuacuitScript.fromCode(code);
            stack.getOrCreateTag().put("luaNbt", ls.serialize());
        } catch (LuaOvertimeException | LuaError e){
            throw new IllegalArgumentException("Failed to compile Lua script: " + e.getMessage());
        }
    }

    public static CompoundTag loadTag(String saveName){
        Path file = LUALINKS.resolve(saveName + ".lua").toAbsolutePath();

        try{
            String code = loadLua(file.toString());

            // LuacuitScript ls = LuacuitScript.fromCode(code);

            return CompoundTagBuilder.create()
                    .withString("code", code)
                    .build();

        } catch (IOException e) {
            throw new RuntimeException(e);
        } catch (LuaOvertimeException | LuaError e){
            throw new IllegalArgumentException("Failed to compile Lua script: " + e.getMessage());
        }
    }

    // Implement saveTag: ensure parent directories exist, write UTF-8 to a temp file then move atomically to target
    public static void saveTag(CompoundTag tag, String uploaded, String uploader){
        Path file = LUALINKS.resolve(uploader).resolve(uploaded + ".lua").toAbsolutePath();

        // LuacuitScript ls = LuacuitScript.deserialize(tag);
        String code = tag.getString("code");//ls.code();

        try {
            Path parent = file.getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }else{
                ControlCraft.LOGGER.error("Fail to get parent when try to save uploaded lua");
                return;
            }

            // Create a temp file in the same directory to allow atomic move on the same filesystem
            Path tmp = Files.createTempFile(parent, uploaded + "-", ".lua.tmp");

            // Write the code as UTF-8
            Files.writeString(tmp, code);

            // Try atomic move, fallback if not supported
            try {
                Files.move(tmp, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            } catch (AtomicMoveNotSupportedException e) {
                Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
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
        }
    }

}

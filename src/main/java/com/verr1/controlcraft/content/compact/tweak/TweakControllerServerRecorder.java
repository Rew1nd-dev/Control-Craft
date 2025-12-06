package com.verr1.controlcraft.content.compact.tweak;

import com.getitemfromblock.create_tweaked_controllers.item.ModItems;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.links.tweakerminal.TweakerminalBlock;
import com.verr1.controlcraft.content.links.tweakerminal.TweakerminalBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Stream;

import static com.verr1.controlcraft.content.links.tweakerminal.TweakerminalBlock.K_POS;

public class TweakControllerServerRecorder {

    public static final Map<UUID, PlayerInput> RECORDED_INPUTS = new HashMap<>();
    // public static final Map<UUID, WorldBlockPos> PLAYER_TO_TERMINAL = new HashMap<>();

    public static void receiveAxis(UUID uuid, List<Double> axisValues){
        if(axisValues.size() != 6)return;
        PlayerInput input = RECORDED_INPUTS.computeIfAbsent(uuid, k -> new PlayerInput());
        for(int i = 0; i < 6; ++i){
            input.axes[i] = axisValues.get(i);
        }
        notifyUpdate(uuid, input);
    }

    public static void receiveButtons(UUID uuid, List<Boolean> buttonValues){
        if(buttonValues.size() != 15)return;
        PlayerInput input = RECORDED_INPUTS.computeIfAbsent(uuid, k -> new PlayerInput());
        for(int i = 0; i < 15; ++i){
            input.buttons[i] = buttonValues.get(i);
        }
        notifyUpdate(uuid, input);
    }


    public static @Nullable WorldBlockPos linkOf(UUID user){
        ServerPlayer player = ControlCraftServer.INSTANCE.getPlayerList().getPlayer(user);
        if(player == null)return null;
        ItemStack stack = player.getMainHandItem();
        if(!stack.is(ModItems.TWEAKED_LINKED_CONTROLLER.get()))return null;
        CompoundTag tag = stack.getOrCreateTag();
        if(!tag.contains(K_POS))return null;
        long bp = tag.getLong(K_POS);
        return WorldBlockPos.of(player.level(), BlockPos.of(bp));
    }

    public static void notifyUpdate(UUID uuid, PlayerInput input){
        WorldBlockPos pos = linkOf(uuid);
        if(pos == null)return;
        BlockEntityGetter.INSTANCE.getBlockEntityAt(pos, TweakerminalBlockEntity.class)
                .filter(be -> be.getUserUUID().equals(uuid))
                .ifPresent(
                        be -> be.updateWith(input)
                );
    }

    public static class PlayerInput{
        public static final PlayerInput EMPTY = new PlayerInput();


        public boolean[] buttons = new boolean[15];
        public double[] axes = new double[6];


        public double lx(){
            return axes[0];
        }

        public double ly(){
            return axes[1];
        }

        public double rx(){
            return axes[2];
        }

        public double ry(){
            return axes[3];
        }

        public double lt(){
            return axes[4];
        }

        public double rt() {
            return axes[5];
        }

        public List<Double> asAxisList(){
            return List.of(lx(), ly(), rx(), ry(), lt(), rt());
        }
    }
}

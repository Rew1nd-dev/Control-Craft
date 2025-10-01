package com.verr1.controlcraft.content.compact.tweak;

import com.verr1.controlcraft.content.links.tweakerminal.TweakerminalBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.server.level.ServerPlayer;

import java.util.*;
import java.util.stream.Stream;

public class TweakControllerServerRecorder {

    public static final Map<UUID, PlayerInput> RECORDED_INPUTS = new HashMap<>();
    public static final Map<UUID, WorldBlockPos> PLAYER_TO_TERMINAL = new HashMap<>();

    public static void receiveAxis(UUID uuid, List<Byte> axisValues){
        if(axisValues.size() != 10)return;
        PlayerInput input = RECORDED_INPUTS.computeIfAbsent(uuid, k -> new PlayerInput());
        for(int i = 0; i < 10; ++i){
            input.axes[i] = axisValues.get(i);
        }
        notifyUpdate(uuid);
    }

    public static void receiveButtons(UUID uuid, List<Boolean> buttonValues){
        if(buttonValues.size() != 15)return;
        PlayerInput input = RECORDED_INPUTS.computeIfAbsent(uuid, k -> new PlayerInput());
        for(int i = 0; i < 15; ++i){
            input.buttons[i] = buttonValues.get(i);
        }
        notifyUpdate(uuid);
    }

    public static void link(UUID user, WorldBlockPos pos){
        PLAYER_TO_TERMINAL.put(user, pos);
        notifyUpdate(user);
    }

    public static void notifyUpdate(UUID uuid){
        WorldBlockPos pos = PLAYER_TO_TERMINAL.get(uuid);
        if(pos == null)return;
        BlockEntityGetter.INSTANCE.getBlockEntityAt(pos, TweakerminalBlockEntity.class)
                .ifPresentOrElse(
                        TweakerminalBlockEntity::update,
                        () -> PLAYER_TO_TERMINAL.remove(uuid)
                );
    }

    public static class PlayerInput{
        public static final PlayerInput EMPTY = new PlayerInput();


        public boolean[] buttons = new boolean[15];
        public double[] axes = new double[10];


        public double lx(){
            return axes[0] - axes[1];
        }

        public double ly(){
            return axes[2] - axes[3];
        }

        public double rx(){
            return axes[4] - axes[5];
        }

        public double ry(){
            return axes[6] - axes[7];
        }

        public double lt(){
            return axes[8];
        }

        public double rt() {
            return axes[9];
        }

        public List<Double> asAxisList(){
            return List.of(lx(), ly(), rx(), ry(), lt(), rt());
        }
    }
}

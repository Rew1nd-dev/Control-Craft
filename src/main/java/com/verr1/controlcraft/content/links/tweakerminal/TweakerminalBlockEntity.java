package com.verr1.controlcraft.content.links.tweakerminal;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.compact.tweak.TweakControllerServerRecorder;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.TweakerminalPlant;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class TweakerminalBlockEntity extends OnShipBlockEntity implements IPlant {


    private UUID userUUID = null;
    private final List<Double> cachedAxis = new ArrayList<>(ArrayUtils.ListOf(10, 0.0));
    private final List<Boolean> cachedButtons = new ArrayList<>(ArrayUtils.ListOf(15, false));
    protected final TweakerminalPlant plant = new TweakerminalPlant(this);

    public TweakerminalBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    public UUID getUserUUID() {
        return userUUID;
    }

    public void setUserUUID(UUID userUUID) {
        this.userUUID = userUUID;
    }

    public void update(){
        TweakControllerServerRecorder.PlayerInput input = TweakControllerServerRecorder.RECORDED_INPUTS.getOrDefault(userUUID, TweakControllerServerRecorder.PlayerInput.EMPTY);
        updateWith(input);

    }

    public void updateWith(TweakControllerServerRecorder.PlayerInput input){
        // make something to commit
        cachedAxis.clear();
        cachedButtons.clear();
        cachedAxis.addAll(input.asAxisList());
        for (int i = 0; i < 15; i++){
            cachedButtons.add(input.buttons[i]);
        };
    }

    @Override
    public void tickServer() {
        super.tickServer();
    }

    public double getAxis(int axisIndex){
        if(userUUID == null || axisIndex < 0 || axisIndex >= cachedAxis.size())return 0;
        return Optional.ofNullable(cachedAxis.get(axisIndex)).orElse(0.0);
    }

    public boolean getButton(int buttonIndex){
        if(userUUID == null || buttonIndex < 0 || buttonIndex >= cachedButtons.size())return false;
        return Optional.ofNullable(cachedButtons.get(buttonIndex)).orElse(false);
    }


    @Override
    public @NotNull NamedComponent plant() {
        return plant;
    }
}

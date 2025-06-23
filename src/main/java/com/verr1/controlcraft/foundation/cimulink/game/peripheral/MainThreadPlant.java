package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.simibubi.create.foundation.blockEntity.SmartBlockEntity;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.executor.Executor;
import net.minecraft.world.level.block.entity.BlockEntity;

import java.util.function.BiConsumer;

public class MainThreadPlant<T extends BlockEntity> extends Plant{

    private final T plant;
    public static Executor ASYNC_SCHEDULER = new Executor();

    protected MainThreadPlant(builder initContext, T plant) {
        super(initContext);
        this.plant = plant;
    }

    public T plant() {
        return plant;
    }

    protected static<T extends SmartBlockEntity, C> void schedule(T plant, C context, BiConsumer<T, C> operation){
        if(ControlCraftServer.onMainThread()){
            operation.accept(plant, context);
        }else {
            ASYNC_SCHEDULER.execute(key(plant), () -> schedule(plant, context, operation));
        }
    }

    private static<T extends SmartBlockEntity> String key(T plant){
        return WorldBlockPos.of(plant.getLevel(), plant.getBlockPos()).toString();
    }

}

package com.verr1.controlcraft.content.cctweaked.peripheral;

import com.verr1.controlcraft.content.blocks.flap.CompactFlapBlockEntity;
import com.verr1.controlcraft.content.blocks.flap.FlapBearingBlockEntity;
import dan200.computercraft.api.lua.LuaFunction;
import dan200.computercraft.api.peripheral.IPeripheral;
import org.jetbrains.annotations.Nullable;

public class CompactFlapPeripheral extends AbstractAttachedPeripheral<CompactFlapBlockEntity> {

    public CompactFlapPeripheral(CompactFlapBlockEntity flapBearingBlockEntity) {
        super(flapBearingBlockEntity);
    }

    @Override
    public boolean equals(@Nullable IPeripheral iPeripheral) {
        if (!(iPeripheral instanceof CompactFlapPeripheral p))return false;
        return getTarget().getBlockPos() == p.getTarget().getBlockPos();
    }

    @Override
    public String getType() {
        return "compact_flap";
    }

    @LuaFunction
    public final double getAngle(){
        return getTarget().angle.read();
    }

    @LuaFunction
    public final void setAngle(double angle){
        getTarget().setAttackAngle(angle);
    }

}
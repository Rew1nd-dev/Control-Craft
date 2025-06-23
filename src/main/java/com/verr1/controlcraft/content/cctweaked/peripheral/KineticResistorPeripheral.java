package com.verr1.controlcraft.content.cctweaked.peripheral;

import com.verr1.controlcraft.content.blocks.kinetic.resistor.KineticResistorBlockEntity;
import dan200.computercraft.api.lua.LuaFunction;
import dan200.computercraft.api.peripheral.IPeripheral;
import org.jetbrains.annotations.Nullable;

public class KineticResistorPeripheral extends AbstractAttachedPeripheral<KineticResistorBlockEntity> {

    public KineticResistorPeripheral(KineticResistorBlockEntity target) {
        super(target);
    }

    @Override
    public String getType() {
        return "resistor";
    }

    @Override
    public boolean equals(@Nullable IPeripheral iPeripheral) {
        if (!(iPeripheral instanceof KineticResistorPeripheral p))return false;
        return getTarget().getBlockPos() == p.getTarget().getBlockPos();
    }

    @LuaFunction
    public final double getRatio(){
        return getTarget().ratio();
    }

    @LuaFunction
    public final void setRatio(double ratio){
        getTarget().setRatio(ratio);
    }


}

package com.verr1.controlcraft.content.cctweaked.peripheral;

import com.verr1.controlcraft.content.links.ccbridge.CCBridgeBlockEntity;
import dan200.computercraft.api.lua.LuaFunction;
import dan200.computercraft.api.peripheral.IPeripheral;
import org.jetbrains.annotations.Nullable;

public class LinkBridgePeripheral extends AbstractAttachedPeripheral<CCBridgeBlockEntity>{

    public LinkBridgePeripheral(CCBridgeBlockEntity target) {
        super(target);
    }

    @Override
    public String getType() {
        return "cc_link_bridge";
    }

    @Override
    public boolean equals(@Nullable IPeripheral iPeripheral) {
        if (!(iPeripheral instanceof LinkBridgePeripheral p))return false;
        return getTarget().getBlockPos() == p.getTarget().getBlockPos();
    }

    @LuaFunction
    public final void setInput(int index, double val){
        getTarget().setToCircuit(index, val);
    }

    @LuaFunction
    public final double getOutput(int index){
        return getTarget().getFromCircuit(index);
    }

}

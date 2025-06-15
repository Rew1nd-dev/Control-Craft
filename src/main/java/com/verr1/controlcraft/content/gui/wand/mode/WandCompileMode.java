package com.verr1.controlcraft.content.gui.wand.mode;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractDualSelectionMode;
import com.verr1.controlcraft.foundation.api.IWandMode;
import com.verr1.controlcraft.foundation.data.WandSelection;
import com.verr1.controlcraft.foundation.network.packets.specific.CimulinkCompilePacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import net.minecraft.core.BlockPos;

public class WandCompileMode extends WandAbstractDualSelectionMode {
    public static final String ID = "circuit_compile";

    public static WandCompileMode instance;

    public static void createInstance(){
        instance = new WandCompileMode();
    }

    @Override
    public IWandMode getInstance() {
        return instance;
    }

    @Override
    public String getID() {
        return ID;
    }

    @Override
    protected void confirm(WandSelection x, WandSelection y) {

        if(x == WandSelection.NULL || y == WandSelection.NULL)return;

        BlockPos sel0 = x.pos();
        BlockPos sel1 = y.pos();

        var p = new CimulinkCompilePacket(sel0, sel1);
        ControlCraftPackets.getChannel().sendToServer(p);
    }
}

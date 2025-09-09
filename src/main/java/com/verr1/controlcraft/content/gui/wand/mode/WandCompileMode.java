package com.verr1.controlcraft.content.gui.wand.mode;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractDualSelectionMode;
import com.verr1.controlcraft.foundation.api.IWandMode;
import com.verr1.controlcraft.foundation.data.WandSelection;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.network.packets.specific.CimulinkCompilePacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.phys.AABB;

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
    public void tick() {
        Player player = Minecraft.getInstance().player;
        if (player == null) return;

        if(x != WandSelection.NULL && y == WandSelection.NULL) {
            ClientOutliner.drawOutline(x.pos(), 0xaaca32, "source");
        }
        if(y != WandSelection.NULL) {
            int min_x = Math.min(x.pos().getX(), y.pos().getX());
            int min_y = Math.min(x.pos().getY(), y.pos().getY());
            int min_z = Math.min(x.pos().getZ(), y.pos().getZ());
            int max_x = Math.max(x.pos().getX(), y.pos().getX());
            int max_y = Math.max(x.pos().getY(), y.pos().getY());
            int max_z = Math.max(x.pos().getZ(), y.pos().getZ());
            ClientOutliner.drawOutline(new AABB(min_x, min_y, min_z, max_x + 1, max_y + 1, max_z + 1), 0xffcb74, "deploy", 0.2f, 0.2f);
            // ClientOutliner.drawOutline(y.pos(), 0xffcb74, "deploy");
        }
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

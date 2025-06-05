package com.verr1.controlcraft.content.gui.wand.mode;

import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractMultipleSelectionMode;
import com.verr1.controlcraft.foundation.api.IWandMode;
import com.verr1.controlcraft.foundation.data.WandSelection;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import com.verr1.controlcraft.foundation.managers.render.CimulinkRenderCenter;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.network.packets.GenericServerPacket;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;

import java.awt.*;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class WandDelinkMode extends WandAbstractMultipleSelectionMode {



    public static WandDelinkMode instance;

    public static void createInstance(){
        instance = new WandDelinkMode();
    }

    @Override
    public IWandMode getInstance() {
        return instance;
    }

    @Override
    public String getID() {
        return "delink";
    }

    @Override
    protected void tick() {
        tickLooking();
    }

    public void tickLooking(){
        Vec3 lookingAtVec = MinecraftUtils.lookingAtVec();
        BlockPos lookingAtPos = MinecraftUtils.lookingAtPos();
        Level level = Minecraft.getInstance().level;
        if(lookingAtPos == null || lookingAtVec == null || level == null)return;
        ClientViewContext cvc = CimulinkRenderCenter.computeContext(lookingAtPos, lookingAtVec, level);
        if(cvc == null)return;
        Color c = cvc.isInput() ? Color.GREEN.darker() : Color.RED.darker();
        ClientOutliner.drawOutline(toMinecraft(MathUtils.centerWithRadius(toJOML(cvc.portPos()), 0.05)), c.getRGB(), "link_looking", 0.4);

    }

    @Override
    public void onSelection(WandSelection selection) {

        ClientViewContext cvc = WandLinkMode.computeContext(selection);
        if (cvc == null) {
            return;
        }

        var p = new GenericServerPacket.builder(RegisteredPacketType.DELINK)
                .withLong(cvc.pos().asLong())
                .withUtf8(cvc.portName())
                .withBoolean(cvc.isInput())
                .build();

        ControlCraftPackets.getChannel().sendToServer(p);
    }
}

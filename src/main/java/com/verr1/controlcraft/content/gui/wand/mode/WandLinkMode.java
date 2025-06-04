package com.verr1.controlcraft.content.gui.wand.mode;

import com.verr1.controlcraft.content.gui.wand.mode.base.WandAbstractDualSelectionMode;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.api.IWandMode;
import com.verr1.controlcraft.foundation.data.WandSelection;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.foundation.data.links.ClientViewContext;
import com.verr1.controlcraft.foundation.managers.CimulinkRenderCenter;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.network.packets.GenericServerPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.CimulinkLinkPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;

import java.awt.*;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class WandLinkMode extends WandAbstractDualSelectionMode {
    public static final String ID = "wand_link_mode";


    public static WandLinkMode instance;

    public static void createInstance(){
        instance = new WandLinkMode();
    }

    @Override
    public IWandMode getInstance() {
        return instance;
    }



    public static @Nullable ClientViewContext computeContext(WandSelection ws){
        if(Minecraft.getInstance().level == null)return null;
        return CimulinkRenderCenter.computeContext(ws.pos(), ws.location(), Minecraft.getInstance().level);
    }



    @Override
    public void tick() {
        tickSelected(x, "x_sel");
        tickSelected(y, "y_sel");
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
        if(cvc.isInput()){
            CimulinkRenderCenter.renderInConnection(cvc.pos(), cvc.portName());
        }else{
            CimulinkRenderCenter.renderOutConnection(cvc.pos(), cvc.portName());
        }
    }



    public void tickSelected(WandSelection sel, String slot){
        if(sel.equals(WandSelection.NULL))return;
        ClientViewContext cvc = computeContext(sel);
        if(cvc == null)return;

        Color c = cvc.isInput() ? Color.GREEN.darker() : Color.RED.darker();
        ClientOutliner.drawOutline(toMinecraft(MathUtils.centerWithRadius(toJOML(cvc.portPos()), 0.05)), c.getRGB(), slot, 0.4);
    }


    @Override
    public String getID() {
        return ID;
    }

    @Override
    protected void confirm(WandSelection x, WandSelection y) {
        if(x == WandSelection.NULL || y == WandSelection.NULL)return;
        ClientViewContext xc = computeContext(x);
        ClientViewContext yc = computeContext(y);
        if(xc == null || yc == null)return;
        if(xc.isInput() == yc.isInput()){
            Optional.ofNullable(Minecraft.getInstance().player).ifPresent(
                    p -> p.sendSystemMessage(Component.literal("can not link 2 input/output port together"))
            );
        }

        BlockPort xp = xc.toPort(Minecraft.getInstance().level);
        BlockPort yp = yc.toPort(Minecraft.getInstance().level);

        var p = xc.isInput() ? new CimulinkLinkPacket(xp, yp) : new CimulinkLinkPacket(yp, xp);

        ControlCraftPackets.getChannel().sendToServer(p);
    }


}

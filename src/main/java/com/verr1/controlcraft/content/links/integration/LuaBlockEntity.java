package com.verr1.controlcraft.content.links.integration;

import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysWorldAccess;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.LuacuitLinkPort;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;

import java.util.List;

public class LuaBlockEntity extends WirelessIntegrationBlockEntity<Luacuit, LuacuitLinkPort>{

    public LuaBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }

    @Override
    protected LuacuitLinkPort create() {
        return new LuacuitLinkPort();
    }

    public void setWorldAccess(){
        linkPort().setWorldAccess(IPhysWorldAccess.of(this));
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        setWorldAccess();
    }

    public void loadCircuit(LuacuitScript nbt) throws IllegalArgumentException{
        var savedStatus = linkPort().viewStatus();
        boolean shouldOpen = linkPort().isEmpty();
        try{
            linkPort().load(nbt);
        }catch (IllegalArgumentException e){
            setChanged();
            throw e;
        }
        linkPort().setStatus(savedStatus);
        if(shouldOpen)linkPort().setToAllOpen();
        updateIOName();
        setChanged();
    }


}

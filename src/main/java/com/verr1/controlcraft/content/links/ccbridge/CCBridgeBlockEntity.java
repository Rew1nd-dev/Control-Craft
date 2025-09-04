package com.verr1.controlcraft.content.links.ccbridge;

import com.verr1.controlcraft.content.cctweaked.peripheral.LinkBridgePeripheral;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.MultiInputLinkPort;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class CCBridgeBlockEntity extends CimulinkBlockEntity<MultiInputLinkPort> {

    private LinkBridgePeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;

    @Override
    protected MultiInputLinkPort create() {
        return new MultiInputLinkPort();
    }

    public CCBridgeBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new LinkBridgePeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }

    public void setToCircuit(int index, double val){
        linkPort().setToCircuit(index, val);
    }

    public double getFromCircuit(int index){
        return linkPort().getFromCircuit(index);
    }


}

package com.verr1.controlcraft.content.links.circuit;

import com.simibubi.create.content.redstone.link.IRedstoneLinkable;
import com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.CircuitLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.items.ItemStackHandler;
import net.minecraftforge.network.NetworkHooks;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static com.simibubi.create.Create.REDSTONE_LINK_NETWORK_HANDLER;
import static com.verr1.controlcraft.ControlCraftServer.DECIMAL_LINK_NETWORK_HANDLER;
import static com.verr1.controlcraft.content.blocks.terminal.TerminalBlockEntity.EMPTY_FREQUENCY;
import static java.lang.Math.min;

public class CircuitBlockEntity extends CimulinkBlockEntity<CircuitLinkPort> implements MenuProvider {

    public static final NetworkKey CIRCUIT = NetworkKey.create("circuit");

    private static final int MAX_CHANNEL_SIZE = 8;
    private final List<WirelessIO> io = new ArrayList<>(ArrayUtils.ListOf(MAX_CHANNEL_SIZE, WirelessIO::new));
    private int validSize = 0;

    private final WrappedChannel wrapper;

    public CircuitBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        wrapper = new WrappedChannel(pos);

        buildRegistry(CIRCUIT)
                .withBasic(SerializePort.of(
                        () -> linkPort().serialize(),
                        t -> linkPort().deserialize(t)
                ))
                .register();
    }

    public void openScreen(Player player){
        wrapper.overrideData(io.subList(0, validSize));
        NetworkHooks.openScreen((ServerPlayer) player, this, wrapper::write);
    }

    @Override
    protected CircuitLinkPort create() {
        return new CircuitLinkPort();
    }

    public void loadCircuit(CircuitNbt nbt){
        linkPort().load(nbt);
        updateIOName();
    }

    private void updateIOName(){
        AtomicInteger ioIndex = new AtomicInteger(0);
        linkPort().inputsNames()
            .forEach(s -> {
                int ioId = ioIndex.get();
                if(ioId >= io.size())return;
                ioIndex.getAndIncrement();

                WirelessIO wirelessIO = io.get(ioId);
                wirelessIO.setAsInput(s);
            });
        linkPort().outputsNames()
            .forEach(s -> {
                int ioId = ioIndex.get();
                if(ioId >= io.size())return;
                ioIndex.getAndIncrement();

                WirelessIO wirelessIO = io.get(ioId);
                wirelessIO.setAsOutput(s);
            });
        for(int i = ioIndex.get(); i < io.size(); i++){
            WirelessIO wirelessIO = io.get(i);
            wirelessIO.setAsRedundant();
        }
        validSize = ioIndex.get();
    }

    private void removeFromNetwork(){
        io.forEach(e -> {
            REDSTONE_LINK_NETWORK_HANDLER.removeFromNetwork(this.level, e);
            // DECIMAL_LINK_NETWORK_HANDLER.removeFromNetwork(this.level, e);
        });
    }

    private void addToNetwork(){
        io.forEach(e -> {
            REDSTONE_LINK_NETWORK_HANDLER.addToNetwork(this.level, e);
            // DECIMAL_LINK_NETWORK_HANDLER.addToNetwork(this.level, e);
        });
    }

    @Override
    protected void initializeExtra() {
        super.initializeExtra();
        addToNetwork();
    }

    @Override
    public void remove() {
        super.remove();
        removeFromNetwork();
    }

    public void updateKeys(List<Couple<RedstoneLinkNetworkHandler.Frequency>> newKeys){
        removeFromNetwork();
        for(int i = 0; i < min(newKeys.size(), io.size()); i++){
            io.get(i).key = newKeys.get(i);
        }
        addToNetwork();
    }

    @Override
    public Component getDisplayName() {
        return Component.literal("circuit");
    }

    @Override
    public @Nullable AbstractContainerMenu createMenu(int id, @NotNull Inventory inv, @NotNull Player player) {
        return null;
    }

    public static ItemStackHandler getFrequencyItems(WrappedChannel contentHolder) {
        ItemStackHandler newInv = new ItemStackHandler(2 * MAX_CHANNEL_SIZE);
        CompoundTag invNBT = contentHolder.inventoryTag().getCompound("items");
        if (!invNBT.isEmpty())
            newInv.deserializeNBT(invNBT);

        if (newInv.getSlots() != 2 * MAX_CHANNEL_SIZE){
            return new ItemStackHandler(2 * MAX_CHANNEL_SIZE);
        }

        return newInv;
    }

    public static Couple<RedstoneLinkNetworkHandler.Frequency> toFrequency(WrappedChannel controller, int slot) {
        ItemStackHandler frequencyItems = getFrequencyItems(controller);
        try {
            return Couple.create(
                    RedstoneLinkNetworkHandler.Frequency.of(frequencyItems.getStackInSlot(2 * slot    )),
                    RedstoneLinkNetworkHandler.Frequency.of(frequencyItems.getStackInSlot(2 * slot + 1))
            );
        }
        catch (IndexOutOfBoundsException e){
            return EMPTY_FREQUENCY;
        }
    }

    public class WirelessIO implements IRedstoneLinkable {
        public Couple<RedstoneLinkNetworkHandler.Frequency> key = EMPTY_FREQUENCY;
        public int strength;

        public int lastReceivedStrength = 0;

        public boolean isInput;
        public String ioName;
        public boolean isRedundant = true;

        public boolean enabled;


        public Couple<Double> minMax = Couple.create(0.0, 1.0);


        @Override
        public int getTransmittedStrength() {
            if(!isInput) return strength;
            return 0;
        }

        public void setAsInput(String name){
            ioName = name;
            isInput = true;
            isRedundant = false;
        }

        public void setAsOutput(String name){
            ioName = name;
            isInput = false;
            isRedundant = false;
        }

        public void setAsRedundant(){
            isRedundant = true;
            isInput = false;
            ioName = "Redundant";
        }

        @Override
        public void setReceivedStrength(int power) {
            if(!isInput)return;

            if (lastReceivedStrength == power)return;
            lastReceivedStrength = power;

            double ratio = power / 15.0;
            double value = minMax.getFirst() + ratio * (minMax.getSecond() - minMax.getFirst());
            try{
                linkPort().input(ioName, value);
            }catch (Exception e){
                ControlCraft.LOGGER.warn("io exception of circuit: " + e.getMessage());
            }

        }

        @Override
        public boolean isListening() {
            return enabled;
        }

        @Override
        public boolean isAlive() {
            return !isRemoved();
        }

        @Override
        public Couple<RedstoneLinkNetworkHandler.Frequency> getNetworkKey() {
            return key;
        }

        @Override
        public BlockPos getLocation() {
            return getBlockPos();
        }
    }
}

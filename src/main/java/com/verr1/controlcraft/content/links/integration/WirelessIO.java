package com.verr1.controlcraft.content.links.integration;

import com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.redstone.$IRedstoneLinkable;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;

import static com.verr1.controlcraft.content.blocks.terminal.TerminalBlockEntity.EMPTY_FREQUENCY;

public class WirelessIO implements $IRedstoneLinkable {
    public Couple<RedstoneLinkNetworkHandler.Frequency> key = EMPTY_FREQUENCY;
    private final IWirelessLinkProvider delegate;

    public int lastReceivedStrength = 0;
    public double $lastReceivedStrength = 0.0;

    public boolean isInput;
    public String ioName = "";
    public boolean isRedundant = true;

    public boolean enabled;


    public Couple<Double> minMax = Couple.create(0.0, 1.0);

    public WirelessIO(IWirelessLinkProvider delegate) {
        this.delegate = delegate;
    }


    @Override
    public int getTransmittedStrength() {
        if (!isInput && !isRedundant) {
            try {
                double out = delegate.linkCircuit().output(ioName);
                return (int) out;
            } catch (Exception e) {
                ControlCraft.LOGGER.error("Error while getting transmitted strength for circuit: {}", e.getMessage());
            }
        }
        return 0;
    }

    public void setAsInput(String name) {
        ioName = name;
        isInput = true;
        isRedundant = false;
    }

    public void setAsOutput(String name) {
        ioName = name;
        isInput = false;
        isRedundant = false;
    }

    public void setAsRedundant() {
        isRedundant = true;
        isInput = false;
        ioName = "Redundant";
    }

    private double select() {
        return delegate.useDecimalNetwork() ? $lastReceivedStrength : lastReceivedStrength;
    }

    private void updateInput() {
        double ratio = select();
        double value = minMax.getFirst() + ratio * (minMax.getSecond() - minMax.getFirst());
        try {
            delegate.linkCircuit().input(ioName, value);
        } catch (Exception e) {
            ControlCraft.LOGGER.warn("io exception of circuit: {}", e.getMessage());
        }
    }

    @Override
    public double $getTransmittedStrength() {
        if (!isInput && !isRedundant && delegate.useDecimalNetwork()) {
            try {
                double out = delegate.linkCircuit().output(ioName);
                return out;
            } catch (Exception e) {
                ControlCraft.LOGGER.error("Error while getting transmitted decimal strength for circuit: {}", e.getMessage());
            }
        }
        return Double.NEGATIVE_INFINITY;
    }

    @Override
    public boolean isSource() {
        return !isInput;
    }

    @Override
    public void setReceivedStrength(int power) {
        if (!isInput || delegate.useDecimalNetwork()) return;

        if (lastReceivedStrength == power) return;
        lastReceivedStrength = power;

        updateInput();
    }

    @Override
    public void $setReceivedStrength(double decimal) {
        if (!isInput || !delegate.useDecimalNetwork()) return;

        if (Math.abs(decimal - $lastReceivedStrength) < 1e-6) return;
        $lastReceivedStrength = decimal;

        updateInput();
    }

    public CompoundTag serialize() {
        CompoundTag tag = new CompoundTag();
        tag.put("key", key.serializeEach(e -> e.getStack().serializeNBT()));
        tag.putString("ioName", ioName);
        tag.putBoolean("isInput", isInput);
        tag.putBoolean("isRedundant", isRedundant);
        tag.putBoolean("enabled", enabled);
        tag.putDouble("min", minMax.getFirst());
        tag.putDouble("max", minMax.getSecond());
        return tag;
    }

    public void deserialize(CompoundTag tag) {
        key = Couple.deserializeEach(tag.getList("key", 10), e -> RedstoneLinkNetworkHandler.Frequency.of(ItemStack.of(e)));
        ioName = tag.getString("ioName");
        isInput = tag.getBoolean("isInput");
        isRedundant = tag.getBoolean("isRedundant");
        enabled = tag.getBoolean("enabled");
        minMax = Couple.create(tag.getDouble("min"), tag.getDouble("max"));
    }

    @Override
    public boolean isListening() {
        return enabled;
    }

    @Override
    public boolean isAlive() {
        return !delegate.isRemoved();
    }

    @Override
    public Couple<RedstoneLinkNetworkHandler.Frequency> getNetworkKey() {
        return key;
    }

    @Override
    public BlockPos getLocation() {
        return delegate.getBlockPos();
    }


}

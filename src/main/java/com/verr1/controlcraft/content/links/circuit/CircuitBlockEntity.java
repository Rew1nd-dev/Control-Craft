package com.verr1.controlcraft.content.links.circuit;

import com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.redstone.$IRedstoneLinkable;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.game.misc.CircuitWirelessMenu;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.CircuitLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.CircuitPortStatus;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.registry.ControlCraftMenuTypes;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
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

    public static Serializer<List<CircuitPortStatus>> CPS_SER =
            SerializeUtils.ofList(
                    SerializeUtils.of(
                            CircuitPortStatus::serialize,
                            CircuitPortStatus::deserialize
                    )
            );

    public static Serializer<Pair<List<CircuitPortStatus>, List<CircuitPortStatus>>> PAIR_SER =
            SerializeUtils.ofPair(CPS_SER);

    public static final NetworkKey CIRCUIT = NetworkKey.create("circuit");
    public static final NetworkKey DECIMAL = NetworkKey.create("decimal");
    public static final NetworkKey CHANNEL = NetworkKey.create("channel");
    public static final NetworkKey WRAPPER = NetworkKey.create("wrapper");

    private static final int MAX_CHANNEL_SIZE = 24;


    private final List<WirelessIO> io = new ArrayList<>(ArrayUtils.ListOf(MAX_CHANNEL_SIZE, WirelessIO::new));

    private int validSize = 0;

    private final WrappedChannel wrapper;



    private boolean useDecimalNetwork = false;

    public CircuitBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        wrapper = new WrappedChannel(pos);

        buildRegistry(CIRCUIT).withBasic(
                CompoundTagPort.of(
                        () -> PAIR_SER.serialize(linkPort().viewStatus()),
                        t -> linkPort().setStatus(PAIR_SER.deserialize(t))
                )
            )
                .withClient(ClientBuffer.UNIT.get())
                .runtimeOnly()
                .register();

        buildRegistry(DECIMAL)
                .withBasic(SerializePort.of(this::useDecimalNetwork, this::setUseDecimalNetwork, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();
        buildRegistry(WRAPPER).withBasic(CompoundTagPort.of(wrapper::saveToTag, wrapper::loadFromTag)).register();
        buildRegistry(CHANNEL).withBasic(CompoundTagPort.of(this::serializeIo, this::deserializeIo)).register();
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

    public boolean useDecimalNetwork() {
        return useDecimalNetwork;
    }

    public void setUseDecimalNetwork(boolean useDecimalNetwork) {
        this.useDecimalNetwork = useDecimalNetwork;
    }

    private Circuit linkCircuit(){
        return linkPort().circuit();
    }


    private void updateIOName(){
        AtomicInteger ioIndex = new AtomicInteger(0);
        linkCircuit().inputsExcludeSignals()
            .forEach(s -> {
                int ioId = ioIndex.get();
                if(ioId >= io.size())return;
                ioIndex.getAndIncrement();

                WirelessIO wirelessIO = io.get(ioId);
                wirelessIO.setAsInput(s);
            });
        linkCircuit().outputs()
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
        io.forEach(e -> {  //  stream().filter(o -> !o.isRedundant)
            DECIMAL_LINK_NETWORK_HANDLER.removeFromNetwork(this.level, e);
            REDSTONE_LINK_NETWORK_HANDLER.removeFromNetwork(this.level, e);
        });
    }

    private void addToNetwork(){
        io.stream().filter(o -> !o.isRedundant).forEach(e -> {
            DECIMAL_LINK_NETWORK_HANDLER.addToNetwork(this.level, e);
            REDSTONE_LINK_NETWORK_HANDLER.addToNetwork(this.level, e);
        });
    }

    private CompoundTag serializeIo(){
        CompoundTag tag = new CompoundTag();
        for (int i = 0; i < MAX_CHANNEL_SIZE; i++) {
            WirelessIO wirelessIO = io.get(i);
            tag.put("io" + i, wirelessIO.serialize());
        }
        return tag;
    }

    private void deserializeIo(CompoundTag tag){
        for (int i = 0; i < MAX_CHANNEL_SIZE; i++) {
            WirelessIO wirelessIO = io.get(i);
            wirelessIO.deserialize(tag.getCompound("io" + i));
        }
    }

    @Override
    protected void initializeExtra() {
        super.initializeExtra();
        addToNetwork();
        updateIOName();
    }

    public void setWithIoSettings(List<IoSettings> settings){
        removeFromNetwork();
        int size = Math.min(settings.size(), io.size());
        for (int i = 0; i < size; i++){
            IoSettings setting = settings.get(i);
            WirelessIO wirelessIO = io.get(i);
            wirelessIO.enabled = setting.enabled();
            wirelessIO.minMax = Couple.create(setting.min(), setting.max());
        }
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

    public void setFrequency(){
        List<Couple<RedstoneLinkNetworkHandler.Frequency>> newKeys = new ArrayList<>();
        for(int i = 0; i < wrapper.ioDatas.size(); i++){
            newKeys.add(toFrequency(wrapper, i));
        }
        updateKeys(newKeys);
        setChanged();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        updateTransmission();
    }

    public void updateTransmission(){
        io.stream().filter(io -> io.enabled && !io.isInput).forEach(wirelessIO -> {
            if(useDecimalNetwork){
                DECIMAL_LINK_NETWORK_HANDLER.updateNetworkOf(level, wirelessIO);
            }else{
                REDSTONE_LINK_NETWORK_HANDLER.updateNetworkOf(level, wirelessIO);
            }
        });
    }

    @Override
    public @NotNull Component getDisplayName() {
        return Component.literal("circuit");
    }

    @Override
    public @Nullable AbstractContainerMenu createMenu(int id, @NotNull Inventory inv, @NotNull Player player) {
        return new CircuitWirelessMenu(ControlCraftMenuTypes.CIRCUIT.get(), id, inv, wrapper);
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

    public class WirelessIO implements $IRedstoneLinkable {
        public Couple<RedstoneLinkNetworkHandler.Frequency> key = EMPTY_FREQUENCY;


        public int lastReceivedStrength = 0;
        public double $lastReceivedStrength = 0.0;

        public boolean isInput;
        public String ioName = "";
        public boolean isRedundant = true;

        public boolean enabled;


        public Couple<Double> minMax = Couple.create(0.0, 1.0);


        @Override
        public int getTransmittedStrength() {
            if(!isInput && !isRedundant) {
                try{
                    double out = linkCircuit().output(ioName);
                    return (int)out;
                } catch (Exception e){
                    ControlCraft.LOGGER.error("Error while getting transmitted strength for circuit: {}", e.getMessage());
                }
            }
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

        private double select(){
            return useDecimalNetwork ? $lastReceivedStrength : lastReceivedStrength;
        }

        private void updateInput(){
            double ratio = select();
            double value = minMax.getFirst() + ratio * (minMax.getSecond() - minMax.getFirst());
            try{
                linkPort().circuit().input(ioName, value);
            }catch (Exception e){
                ControlCraft.LOGGER.warn("io exception of circuit: " + e.getMessage());
            }
        }

        @Override
        public double $getTransmittedStrength() {
            if(!isInput && !isRedundant && useDecimalNetwork) {
                try{
                    double out = linkCircuit().output(ioName);
                    return out;
                } catch (Exception e){
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
            if(!isInput || useDecimalNetwork)return;

            if (lastReceivedStrength == power)return;
            lastReceivedStrength = power;

            updateInput();
        }

        @Override
        public void $setReceivedStrength(double decimal) {
            if(!isInput || !useDecimalNetwork)return;

            if (Math.abs(decimal - $lastReceivedStrength) < 1e-6)return;
            $lastReceivedStrength = decimal;

            updateInput();
        }

        public CompoundTag serialize(){
            CompoundTag tag = new CompoundTag();
            tag.put("key",  key.serializeEach(e -> e.getStack().serializeNBT()));
            tag.putString("ioName", ioName);
            tag.putBoolean("isInput", isInput);
            tag.putBoolean("isRedundant", isRedundant);
            tag.putBoolean("enabled", enabled);
            tag.putDouble("min", minMax.getFirst());
            tag.putDouble("max", minMax.getSecond());
            return tag;
        }

        public void deserialize(CompoundTag tag){
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

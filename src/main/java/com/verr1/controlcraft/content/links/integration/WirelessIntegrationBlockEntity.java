package com.verr1.controlcraft.content.links.integration;

import com.simibubi.create.content.redstone.link.RedstoneLinkNetworkHandler;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.misc.CircuitWirelessMenu;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.PlantProxy;
import com.verr1.controlcraft.foundation.cimulink.game.port.packaged.WrappedLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.IntegrationPortStatus;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
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

public abstract class WirelessIntegrationBlockEntity<W extends NamedComponent, T extends WrappedLinkPort<W>> extends CimulinkBlockEntity<T>
    implements MenuProvider, IWirelessLinkProvider
{


    public static Serializer<List<IntegrationPortStatus>> CPS_SER =
            SerializeUtils.ofList(
                    SerializeUtils.of(
                            IntegrationPortStatus::serialize,
                            IntegrationPortStatus::deserialize
                    )
            );

    public static Serializer<Pair<List<IntegrationPortStatus>, List<IntegrationPortStatus>>> PAIR_SER =
            SerializeUtils.ofPair(CPS_SER);

    public static final NetworkKey CIRCUIT = NetworkKey.create("circuit");
    public static final NetworkKey DECIMAL = NetworkKey.create("decimal");
    public static final NetworkKey CHANNEL = NetworkKey.create("channel");
    public static final NetworkKey WRAPPER = NetworkKey.create("wrapper");

    private static final int MAX_CHANNEL_SIZE = 24;


    private final List<WirelessIO> io = new ArrayList<>(ArrayUtils.ListOf(MAX_CHANNEL_SIZE, () -> new WirelessIO(this)));

    private int validSize = 0;

    private final WrappedChannel wrapper;
    private boolean useDecimalNetwork = false;

    protected WirelessIntegrationBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
        wrapper = new WrappedChannel(pos);

        buildRegistry(CIRCUIT).withBasic(
                        CompoundTagPort.of(
                                () -> PAIR_SER.serialize(linkPort().viewStatus()),
                                t -> setStatus(PAIR_SER.deserialize(t))
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

    public boolean useDecimalNetwork() {
        return useDecimalNetwork;
    }

    public void setUseDecimalNetwork(boolean useDecimalNetwork) {
        this.useDecimalNetwork = useDecimalNetwork;
    }

    public W linkCircuit(){
        return linkPort().component();
    }

    public PlantProxy linkProxy(){
        return linkPort().proxy();
    }

    protected void setStatus(Pair<List<IntegrationPortStatus>, List<IntegrationPortStatus>> statues){
        linkPort().setStatus(statues);
        updateIOName();
    }

    protected void updateIOName(){
        AtomicInteger ioIndex = new AtomicInteger(0);
        linkProxy().inputsExcludeSignals()
                .forEach(s -> {
                    int ioId = ioIndex.get();
                    if(ioId >= io.size())return;
                    ioIndex.getAndIncrement();

                    WirelessIO wirelessIO = io.get(ioId);
                    wirelessIO.setAsInput(s);
                });
        linkProxy().outputs()
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

    protected void removeFromNetwork(){
        io.forEach(e -> {  //  stream().filter(o -> !o.isRedundant)
            DECIMAL_LINK_NETWORK_HANDLER.removeFromNetwork(this.level, e);
            REDSTONE_LINK_NETWORK_HANDLER.removeFromNetwork(this.level, e);
        });
    }

    protected void addToNetwork(){
        io.stream().filter(o -> !o.isRedundant).forEach(e -> {
            DECIMAL_LINK_NETWORK_HANDLER.addToNetwork(this.level, e);
            REDSTONE_LINK_NETWORK_HANDLER.addToNetwork(this.level, e);
        });
    }

    protected CompoundTag serializeIo(){
        CompoundTag tag = new CompoundTag();
        for (int i = 0; i < MAX_CHANNEL_SIZE; i++) {
            WirelessIO wirelessIO = io.get(i);
            tag.put("io" + i, wirelessIO.serialize());
        }
        return tag;
    }

    protected void deserializeIo(CompoundTag tag){
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

}

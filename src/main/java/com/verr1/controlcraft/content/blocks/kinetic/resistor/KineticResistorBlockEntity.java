package com.verr1.controlcraft.content.blocks.kinetic.resistor;

import com.simibubi.create.content.kinetics.transmission.SplitShaftBlockEntity;
import com.simibubi.create.foundation.utility.Couple;
import com.simibubi.create.infrastructure.config.AllConfigs;
import com.simibubi.create.infrastructure.config.CKinetics;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.cctweaked.peripheral.KineticResistorPeripheral;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.ResistorPlant;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.network.handler.NetworkHandler;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.simibubi.create.content.kinetics.base.DirectionalKineticBlock.FACING;

public class KineticResistorBlockEntity extends SplitShaftBlockEntity implements
        IReceiver, INetworkHandle, IPlant
{
    public static final NetworkKey RATIO = NetworkKey.create("ratio");

    private double ratio = 1.0;

    private final DirectReceiver receiver = new DirectReceiver();

    private final NetworkHandler handler = new NetworkHandler(this);

    private final ResistorPlant plant = new ResistorPlant(this);

    private KineticResistorPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    public KineticResistorBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

        handler
                .buildRegistry(FIELD)
                .withBasic(CompoundTagPort.of(
                        () -> receiver().serialize(),
                        t -> receiver().deserialize(t)
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.UNIT, CompoundTag.class)
                )
                .dispatchToSync()
                .register();

        handler
                .buildRegistry(RATIO)
                .withBasic(SerializePort.of(
                        this::ratio,
                        t -> {
                            setRatioOnly(t);
                            ControlCraftServer.SERVER_EXECUTOR.executeLater(this::refreshKinetics, 2);
                        },
                        SerializeUtils.DOUBLE
                ))
                .withClient(
                        ClientBuffer.DOUBLE.get()
                ).register();

        receiver().register(
                new NumericField(
                        this::ratio,
                        this::setRatio,
                        "Ratio"
                ),
                new DirectReceiver.InitContext(SlotType.RATIO, Couple.create(0.0, 1.0)),
                new DirectReceiver.InitContext(SlotType.RATIO, Couple.create(0.0, 1.0)),
                new DirectReceiver.InitContext(SlotType.RATIO, Couple.create(0.0, 1.0)),
                new DirectReceiver.InitContext(SlotType.RATIO, Couple.create(0.0, 1.0)),
                new DirectReceiver.InitContext(SlotType.RATIO, Couple.create(0.0, 1.0)),
                new DirectReceiver.InitContext(SlotType.RATIO, Couple.create(0.0, 1.0))
        );

    }

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new KineticResistorPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }

    @Override
    public int getFlickerScore() {
        return 0;
    }

    public double ratio() {
        return ratio;
    }

    public void setRatio(double ratio) {
        setRatioOnly(ratio);
        refreshKinetics();
    }

    private void refreshKinetics(){
        detachKinetics();
        attachKinetics();
    }

    private void setRatioOnly(double ratio){
        this.ratio = MathUtils.clamp(ratio, 2);
        setChanged();
    }

    @Override
    public float getRotationSpeedModifier(Direction face) {
        if (hasSource() && face == getBlockState().getValue(FACING).getOpposite()) {
            return clampedModifier();
        }
        return 1;
    }

    public float clampedModifier(){
        if(Math.abs(getTheoreticalSpeed()) < 1e-4){
            return 0.0f;
        }
        float newSpeed = Math.min(getTheoreticalSpeed() * (float) ratio, AllConfigs.server().kinetics.maxRotationSpeed.get());
        return ((int) newSpeed) / getTheoreticalSpeed();
    }

    @Override
    protected void read(CompoundTag compound, boolean clientPacket) {
        super.read(compound, clientPacket);
        handler.onRead(compound, clientPacket);
    }

    @Override
    protected void write(CompoundTag compound, boolean clientPacket) {
        super.write(compound, clientPacket);
        handler.onWrite(compound, clientPacket);
    }

    @Override
    public String name() {
        return "resistor";
    }

    @Override
    public NetworkHandler handler() {
        return handler;
    }

    @Override
    public @NotNull NamedComponent plant() {
        return plant;
    }
}

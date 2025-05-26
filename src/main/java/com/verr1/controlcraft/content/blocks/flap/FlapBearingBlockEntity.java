package com.verr1.controlcraft.content.blocks.flap;

import com.simibubi.create.AllSoundEvents;
import com.simibubi.create.content.contraptions.AbstractContraptionEntity;
import com.simibubi.create.content.contraptions.AssemblyException;
import com.simibubi.create.content.contraptions.ControlledContraptionEntity;
import com.simibubi.create.content.contraptions.bearing.BearingBlock;
import com.simibubi.create.content.contraptions.bearing.BearingContraption;
import com.simibubi.create.content.contraptions.bearing.IBearingBlockEntity;
import com.simibubi.create.content.equipment.goggles.IHaveGoggleInformation;
import com.simibubi.create.foundation.utility.Couple;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.SharedKeys;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.content.cctweaked.peripheral.FlapBearingPeripheral;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.api.delegate.ITerminalDevice;
import com.verr1.controlcraft.foundation.data.WingContraption;
import com.verr1.controlcraft.foundation.data.field.ExposedFieldWrapper;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundClientPacket;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundServerPacket;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.util.Mth;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.block.state.properties.BlockStateProperties;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.PacketDistributor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class FlapBearingBlockEntity extends OnShipBlockEntity implements
        IBearingBlockEntity, IReceiver, IPacketHandler, IHaveGoggleInformation
{
    public static NetworkKey ANGLE = NetworkKey.create("angle");

    protected ControlledContraptionEntity physicalWing;
    protected LerpedFloat clientAnimatedAngle = LerpedFloat.angular();
    protected float angle;
    protected float adjustSpeed;
    protected boolean running;


    private FlapBearingPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;

    private final DirectReceiver receiver = new DirectReceiver();

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new FlapBearingPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }

    public LerpedFloat getClientAnimatedAngle() {
        return clientAnimatedAngle;
    }

    public void assemble(){
        if(isAssembled())return;
        if (level == null || !(level.getBlockState(worldPosition).getBlock() instanceof BearingBlock))
            return;

        Direction direction = getBlockState().getValue(BearingBlock.FACING);
        WingContraption wingContraption = new WingContraption(direction);

        AssemblyException lastException;
        try {
            if (!wingContraption.assemble(level, worldPosition))
                return;

            lastException = null;
        } catch (AssemblyException e) {
            lastException = e;
            ControlCraft.LOGGER.info(e.toString());
            sendData();
            return;
        }

        running = true;
        wingContraption.removeBlocksFromWorld(level, BlockPos.ZERO);
        physicalWing = ControlledContraptionEntity.create(level, this, wingContraption);
        BlockPos anchor = worldPosition.relative(direction);
        physicalWing.setPos(anchor.getX(), anchor.getY(), anchor.getZ());
        physicalWing.setRotationAxis(direction.getAxis());
        level.addFreshEntity(physicalWing);

        AllSoundEvents.CONTRAPTION_ASSEMBLE.playOnServer(level, worldPosition);
        angle = 0;
        sendData();

    }

    public void disassemble() {
        if (!isAssembled()) return;
        angle = 0;
        running = false;
        physicalWing.disassemble();
        AllSoundEvents.CONTRAPTION_DISASSEMBLE.playOnServer(level, worldPosition);
        physicalWing = null;
        sendData();
    }

    public boolean isAssembled(){
        return physicalWing != null;
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        syncForNear(true, ANGLE, FIELD_);
        if (physicalWing != null){
            sendData();
        }
    }

    @Override
    public void destroy() {
        if(level == null || !level.isClientSide){
            disassemble();
        }
        super.destroy();
    }

    @Override
    public void remove() {
        if(level == null || !level.isClientSide){
            disassemble();
        }
        super.remove();
    }

    @Override
    public void tick() {
        super.tick();
        if(physicalWing == null)return;
        applyRotation();
        tickAnimationData();

    }

    @Override
    public void invalidate(){
        super.invalidate();
        if(peripheralCap != null){
            peripheralCap.invalidate();
            peripheralCap = null;
        }
    }

    protected void applyRotation() {
        if(level == null)return;
        float wingAngle = level.isClientSide ? clientAnimatedAngle.getValue() : angle;
        if (physicalWing == null)
            return;
        physicalWing.setAngle(wingAngle);
        BlockState blockState = getBlockState();
        if (blockState.hasProperty(BlockStateProperties.FACING))
            physicalWing.setRotationAxis(
                    blockState
                            .getValue(BlockStateProperties.FACING)
                            .getAxis()
            );
    }


    @Override
    public boolean isAttachedTo(AbstractContraptionEntity contraption) {
        return contraption == physicalWing;
    }

    @Override
    public void attach(ControlledContraptionEntity contraption) {
        BlockState blockState = getBlockState();
        if (!(contraption.getContraption() instanceof BearingContraption))
            return;
        if (!blockState.hasProperty(BearingBlock.FACING))
            return;

        this.physicalWing = contraption;
        setChanged();
        BlockPos anchor = worldPosition.relative(blockState.getValue(BearingBlock.FACING));
        physicalWing.setPos(anchor.getX(), anchor.getY(), anchor.getZ());
        if (!level.isClientSide) {
            sendData();
        }
    }

    @Override
    public void onStall() {
        if (level == null || !level.isClientSide)
            sendData();
    }

    @Override
    public boolean isValid() {
        return isRemoved();
    }

    @Override
    public BlockPos getBlockPosition() {
        return getBlockPos();
    }

    @Override
    public float getInterpolatedAngle(float partialTicks) {
        return Mth.lerp(partialTicks, angle, angle + adjustSpeed * 0.05f);
    }

    @Override
    public boolean isWoodenTop() {
        return false;
    }

    @Override
    public void setAngle(float v) {
        setAngle((double)v);
    }


    public void setAngle(double forcedAngle) {
        angle = MathUtils.angleReset((float) forcedAngle);
    }

    public double getAngle() {
        return angle;
    }

    private void tickAnimationData(){


        if(level == null || !level.isClientSide)return;
        clientAnimatedAngle.chase(angle, 0.1, LerpedFloat.Chaser.EXP);
        clientAnimatedAngle.tickChaser();
    }




    protected void displayScreen(ServerPlayer player){
        double angle = getAngle();

        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.OPEN_SCREEN_0)
                .withDouble(angle)
                .build();

        ControlCraftPackets.sendToPlayer(p, player);
    }

    @Override
    public boolean addToGoggleTooltip(List<Component> tooltip, boolean isPlayerSneaking) {
        return receiver().makeToolTip(tooltip, isPlayerSneaking);
    }



    @Override
    public String name() {
        return "Wing Controller";
    }


    @Override
    public void handleServer(NetworkEvent.Context context, BlockBoundServerPacket packet) {
        if(packet.getType() == RegisteredPacketType.SETTING_0){
            double angle = packet.getDoubles().get(0);
            setAngle((float)angle);
        }
    }

    public void setAssemble(boolean toAssemble){
        if (toAssemble){
            assemble();
        }else{
            disassemble();
        }
    }

    public FlapBearingBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);

        buildRegistry(FIELD_)
                .withBasic(CompoundTagPort.of(
                        () -> receiver().serialize(),
                        t -> receiver().deserialize(t)
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.UNIT, CompoundTag.class)
                )
                .dispatchToSync()
                .register();

        buildRegistry(ANGLE)
                .withBasic(SerializePort.of(
                        this::getAngle,
                        this::setAngle,
                        SerializeUtils.DOUBLE
                ))
                .withClient(ClientBuffer.DOUBLE.get())
                .dispatchToSync()
                .runtimeOnly()
                .register();

        panel().registerUnit(SharedKeys.ASSEMBLE, this::assemble);

        panel().registerUnit(SharedKeys.DISASSEMBLE, this::disassemble);

        receiver().register(
                new NumericField(
                        this::getAngle,
                        this::setAngle,
                        "angle"
                ),
                new DirectReceiver.InitContext(SlotType.DEGREE, Couple.create(0.0, 1.0)),
                8
        ).register(
                new NumericField(
                        () -> isAssembled() ? 1.0 : 0.0,
                        t -> {
                            if (t > 0.001) assemble();
                            else disassemble();
                        },
                        "assembly"
                ),
                new DirectReceiver.InitContext(SlotType.IS_ASSEMBLED, Couple.create(0.0, 1.0)),
                2
        );


        lazyTickRate = 3;
    }
}

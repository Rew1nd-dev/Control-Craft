package com.verr1.controlcraft.content.blocks.camera;

import com.simibubi.create.CreateClient;
import com.simibubi.create.content.equipment.goggles.IHaveGoggleInformation;
import com.simibubi.create.foundation.utility.Color;
import com.simibubi.create.foundation.utility.Couple;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.camera.CameraBoundFakePlayer;
import com.verr1.controlcraft.foundation.camera.CameraMovementTracker;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.CameraPlant;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.NumericField;
import com.verr1.controlcraft.foundation.data.render.RayLerpHelper;
import com.verr1.controlcraft.foundation.managers.ClientCameraManager;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.CompoundTagPort;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.content.cctweaked.peripheral.CameraPeripheral;
import com.verr1.controlcraft.foundation.api.IPacketHandler;
import com.verr1.controlcraft.foundation.data.ShipHitResult;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundClientPacket;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundServerPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.ReceiveLatestWorldPosPacket;
import com.verr1.controlcraft.foundation.redstone.DirectReceiver;
import com.verr1.controlcraft.foundation.redstone.IReceiver;
import com.verr1.controlcraft.foundation.type.descriptive.CameraClipType;
import com.verr1.controlcraft.foundation.type.descriptive.SlotType;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.mixinducks.IEntityDuck;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import com.verr1.controlcraft.utils.*;
import dan200.computercraft.api.peripheral.IPeripheral;
import dan200.computercraft.shared.Capabilities;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.Vec3i;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.ClipContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.levelgen.structure.BoundingBox;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.EntityHitResult;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.PacketDistributor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.*;
import org.joml.primitives.AABBd;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.Ship;
import org.valkyrienskies.core.api.ships.properties.ShipTransform;
import org.valkyrienskies.mod.common.VSGameUtilsKt;
import org.valkyrienskies.mod.common.util.VectorConversionsMCKt;
import org.valkyrienskies.mod.common.world.RaycastUtilsKt;

import java.lang.Math;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class CameraBlockEntity extends OnShipBlockEntity
        implements IPacketHandler, IReceiver, IHaveGoggleInformation, IPlant
{
    public static NetworkKey RESET = NetworkKey.create("reset_view");

    public static NetworkKey PITCH = NetworkKey.create("pitch");
    public static NetworkKey YAW = NetworkKey.create("yaw");
    public static NetworkKey IS_ACTIVE_SENSOR = NetworkKey.create("sensor");

    public static NetworkKey RAY_TYPE = NetworkKey.create("ray_type");
    public static NetworkKey SHIP_TYPE = NetworkKey.create("ship_type");
    public static NetworkKey ENTITY_TYPE = NetworkKey.create("entity_type");


    public ShipHitResult latestShipHitResult = null;
    public EntityHitResult latestEntityHitResult = null;
    public EntityHitResult latestServerPlayerHitResult = null;
    public BlockHitResult latestBlockHitResult = null;

    public RayLerpHelper rayLerpHelper = new RayLerpHelper();

    public static ClipContext EMPTY = new ClipContext(
            new Vec3(0, 0, 0),
            new Vec3(0, 0, 0),
            ClipContext.Block.OUTLINE,
            ClipContext.Fluid.NONE,
            null
    );

    private double pitch = 0; // in degree
    private double yaw = 0;



    private CameraClipType rayType = CameraClipType.NO_RAY;

    private final CameraPlant plant = new CameraPlant(this);

    public final CameraMovementTracker tracker;

    private CameraClipType shipType = CameraClipType.SHIP_CLIP_OFF;

    private CameraClipType entityType = CameraClipType.ENTITY_OFF;



    private CameraPeripheral peripheral;
    private LazyOptional<IPeripheral> peripheralCap;

    private double clipRange = 256;

    private double coneAngle = 0.2;


    private boolean receivedSignalChanged = false;
    private int lastOutputSignal = 0;

    private boolean isActiveDistanceSensor = false;

    private final DirectReceiver receiver = new DirectReceiver();

    private CameraBoundFakePlayer fp;

    public void clipNewShip(){
        latestShipHitResult = clipShip();
    }

    public void clipNewEntity(){
        latestEntityHitResult = clipEntity(Entity::isAlive);
    }

    public void clipNewEntityInView(){
        latestEntityHitResult = clipEntityInView(
                e ->
                e instanceof LivingEntity lv &&
                        e.isAlive() &&
                        !e.isRemoved() &&
                        lv.getHealth() > 0.1
        );
    }

    public CameraClipType rayType() {
        return rayType;
    }

    public void setRayType(CameraClipType rayType) {
        this.rayType = rayType;
    }

    public CameraClipType shipType() {
        return shipType;
    }

    public void setShipType(CameraClipType shipType) {
        this.shipType = shipType;
    }

    public CameraClipType entityType() {
        return entityType;
    }

    public void setEntityType(CameraClipType entityType) {
        this.entityType = entityType;
    }

    public void clipNewServerPlayer(){
        latestServerPlayerHitResult = clipServerPlayer();
    }

    public void clipNewBlock(){
        latestBlockHitResult = clipBlock(false);
    }

    public double getClipRange() {
        return clipRange;
    }

    public void setClipRange(double clipRange) {
        this.clipRange = clipRange;
    }

    public boolean isActiveDistanceSensor() {
        return isActiveDistanceSensor;
    }

    public void setActiveDistanceSensor(boolean activeDistanceSensor) {
        isActiveDistanceSensor = activeDistanceSensor;
    }

    public void setConeAngle(double coneAngle) {
        this.coneAngle = coneAngle;
    }

    @Override
    public DirectReceiver receiver() {
        return receiver;
    }

    public String getUserUUID() {
        if(level == null || level.isClientSide)return "";
        return ServerCameraManager.getUserUUID(WorldBlockPos.of(level, worldPosition)).toString();
    }


    public void updateNeighbor(){
        if(level == null)return;
        if (!receivedSignalChanged)return;
        receivedSignalChanged = false;
        Arrays
            .stream(Direction.values())
            .filter(receiver().view().get(0).view().get(0).direction::test)
            .forEach(
                    face -> {
                        BlockPos attachedPos = worldPosition.relative(face);
                        level.blockUpdated(worldPosition, level.getBlockState(worldPosition)
                                .getBlock());
                        level.blockUpdated(attachedPos, level.getBlockState(attachedPos)
                                .getBlock());
                    }
            );
    }

    public double getClipDistance(){
        BlockHitResult r = clipBlock(true);
        if(r == null)return 0;
        Vector3d p = getCameraPosition();
        Vector3d q = ValkyrienSkies.set(new Vector3d(), r.getLocation());
        return p.distance(q);
    }

    @Override
    public void onChunkUnloaded() {
        super.onChunkUnloaded();
        if(level.isClientSide){
            ControlCraft.LOGGER.info("Unloading Camera: {}", getBlockPos());
        }
    }

    public int getOutputSignal(){
        return lastOutputSignal;
    }

    public void updateOutputSignal(){
        if(!isActiveDistanceSensor)return;
        if(level == null || level.isClientSide)return;
        // if(!fields.get(0).directionOptional.test(side))return 0;

        double d = getClipDistance();

        double a = receiver().view().get(0).view().get(0).min_max.get(true);
        double b = receiver().view().get(0).view().get(0).min_max.get(false);
        double ratio = MathUtils.clampHalf(
                Math.abs(d - a) / (Math.abs(a - b) + 1e-8), 1
        );

        int newSignal = (int)(ratio * 15);
        if(newSignal != lastOutputSignal){
            receivedSignalChanged = true;
            lastOutputSignal = newSignal;
            setChanged();
        }

    }

    public void setPitch(double pitch) {
        this.pitch = pitch;
    }

    public void setYaw(double yaw) {
        this.yaw = yaw;
    }

    public void setPitchYaw(double pitch, double yaw){
        setPitch(pitch);
        setYaw(yaw);
    }

    @OnlyIn(Dist.CLIENT)
    public boolean isLinkedCamera(){
        return Optional
                .ofNullable(ClientCameraManager.getLinkCameraPos())
                .map(c -> c.equals(getBlockPos())).orElse(false);
    }

    @OnlyIn(Dist.CLIENT)
    public void setPitchForceClient(double pitch){
        setPitch(pitch);
        if(!isLinkedCamera())return;
        LocalPlayer player = Minecraft.getInstance().player;
        if(player == null)return;
        player.setXRot((float)pitch);
    }

    @OnlyIn(Dist.CLIENT)
    public void setYawForceClient(double yaw){
        setYaw(yaw);
        if(!isLinkedCamera())return;
        LocalPlayer player = Minecraft.getInstance().player;
        if(player == null)return;
        player.setYRot((float)yaw);
    }

    public void setPitchYawForceServer(double pitch, double yaw){
        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.SYNC_1)
                .withDouble(pitch)
                .withDouble(yaw)
                .build();
        setPitch(pitch);
        setYaw(yaw);
        ControlCraftPackets.getChannel().send(PacketDistributor.ALL.noArg(), p);
    }


    public double getPitch() {
        return pitch;
    }

    public double getYaw() {
        return yaw;
    }



    @Override
    public @NotNull <T> LazyOptional<T> getCapability(@NotNull Capability<T> cap, @Nullable Direction side) {
        if(cap == Capabilities.CAPABILITY_PERIPHERAL){
            if(this.peripheral == null){
                this.peripheral = new CameraPeripheral(this);
            }
            if(peripheralCap == null || !peripheralCap.isPresent())
                peripheralCap =  LazyOptional.of(() -> this.peripheral);
            return peripheralCap.cast();
        }
        return super.getCapability(cap, side);
    }



    private @NotNull ClipContext clipContext(){
        if(level == null)return EMPTY;
        Vector3dc camPos_wc = getCameraPosition(); // VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, worldPosition));
        Vector3dc camFront_wc = getAbsViewForward();
        Vector3dc camStart_wc = camPos_wc.add(camFront_wc, new Vector3d());
        Vector3dc camTo_wc = camStart_wc.fma(clipRange, camFront_wc, new Vector3d());
        return new ClipContext(
                ValkyrienSkies.toMinecraft(camStart_wc),
                ValkyrienSkies.toMinecraft(camTo_wc),
                ClipContext.Block.OUTLINE,
                ClipContext.Fluid.NONE,
                null
        );
    }

    private @NotNull ClipContext clipContextShip(){
        if(level == null)return EMPTY;
        Vector3dc camPos_wc = getCameraPositionShip(); // VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, worldPosition));
        Vector3dc camFront_wc = getLocViewForward();
        Vector3dc camStart_wc = camPos_wc.add(camFront_wc, new Vector3d());
        Vector3dc camTo_wc = camStart_wc.fma(clipRange, camFront_wc, new Vector3d());
        return new ClipContext(
                ValkyrienSkies.toMinecraft(camStart_wc),
                ValkyrienSkies.toMinecraft(camTo_wc),
                ClipContext.Block.OUTLINE,
                ClipContext.Fluid.NONE,
                null
        );
    }


    private @NotNull AABB clipAABB(){
        if(level == null)return AABB.of(BoundingBox.fromCorners(new Vec3i(0, 0, 0), new Vec3i(0, 0, 0)));
        Vector3dc center = getCameraPosition(); // VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, worldPosition));
        Vector3dc view = getAbsViewForward();
        Vector3dc camMin = center.fma(clipRange, view, new Vector3d());
        Vector3dc camMax = center.fma(-10, view, new Vector3d());
        return new AABB(
                ValkyrienSkies.toMinecraft(camMin),
                ValkyrienSkies.toMinecraft(camMax)
        );
    }

    private @NotNull AABB trivialAABB(){
        if(level == null)return AABB.of(BoundingBox.fromCorners(new Vec3i(0, 0, 0), new Vec3i(0, 0, 0)));
        Vector3dc center = getCameraPosition(); // VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, worldPosition));
        return new AABB(
                ValkyrienSkies.toMinecraft(center.add(new Vector3d(clipRange, clipRange, clipRange), new Vector3d())),
                ValkyrienSkies.toMinecraft(center.add(new Vector3d(-clipRange, -clipRange, -clipRange), new Vector3d()))
        );
    }

    private @NotNull AABB coneAABB(){
        if(level == null)return AABB.of(BoundingBox.fromCorners(new Vec3i(0, 0, 0), new Vec3i(0, 0, 0)));
        Vector3dc center = getCameraPosition(); // VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, worldPosition));
        Vector3dc view = getAbsViewForward();
        Vector3dc camEnd = center.fma(clipRange * 0.7, view, new Vector3d());
        double radiusSquare = Math.tan(coneAngle) * clipRange;
        AABBd endAABB = MathUtils.centerWithRadius(camEnd, radiusSquare).intersection(toJOML(trivialAABB()));
        ArrayList<Vector3dc> points = new ArrayList<>(MathUtils.pointOf(endAABB));
        points.add(center);
        return ValkyrienSkies.toMinecraft(MathUtils.coverOf(points));

    }



    public void resetView(){
        if(level == null || level.isClientSide)return;
        switch (getDirection()){
            case UP -> setPitchYawForceServer(-90, 0);
            case DOWN -> setPitchYawForceServer(90, 0);
            case SOUTH -> setPitchYawForceServer(0, 0);
            case NORTH -> setPitchYawForceServer(0, 180);
            case EAST -> setPitchYawForceServer(0, -90);
            case WEST -> setPitchYawForceServer(0, 90);
        }
    }


    public BlockHitResult transformToWorld(@Nullable BlockHitResult hitResult, Level level){
        if(hitResult == null)return null;
        if(!VSGameUtilsKt.isBlockInShipyard(level, hitResult.getBlockPos()))return hitResult;
        Ship ship = VSGameUtilsKt.getShipManagingPos(level, hitResult.getBlockPos());
        if(ship == null)return hitResult;
        Vector3d v = ship.getTransform().getShipToWorld().transformPosition(toJOML(hitResult.getLocation()));
        return new BlockHitResult(
                new Vec3(v.x, v.y, v.z),
                hitResult.getDirection(),
                hitResult.getBlockPos(),
                hitResult.isInside()
        );
    }


    public @Nullable BlockHitResult clipBlock(Vector3d from, Vector3d to){
        if(level == null)return null;
        ClipContext context = new ClipContext(
                ValkyrienSkies.toMinecraft(from),
                ValkyrienSkies.toMinecraft(to),
                ClipContext.Block.OUTLINE,
                ClipContext.Fluid.NONE,
                null
        );
        return RaycastUtilsKt.clipIncludeShips(level, context);
    }

    public @Nullable ShipHitResult clipShip(){
        ClipContext context = clipContext();
        return ClipUtils.clipShip(
                context.getFrom(),
                context.getTo(),
                coneAABB(),
                3,
                level,
                s -> !Optional.ofNullable(getShipOn()).map(Ship::getId).orElse(0L).equals(s.getId())
        );
    }

    public @Nullable EntityHitResult clipServerPlayer(){
        ClipContext context = clipContext();
        return ClipUtils.clipServerPlayer(
                context.getFrom(),
                context.getTo(),
                coneAABB(),
                3,
                (ServerLevel)level,
                p -> true
        );
    }

    public @Nullable BlockHitResult clipBlock(boolean transformToWorld){
        if(level == null)return null;

        return RaycastUtilsKt.clipIncludeShips(level, clipContext(), transformToWorld, getShipOrGroundID());
    }

    public @Nullable EntityHitResult clipEntity(Predicate<Entity> filter){
        if(level == null)return null;
        ClipContext context = clipContext();
        return ClipUtils.clipEntity(
                context.getFrom(),
                context.getTo(),
                coneAABB(),
                coneAngle,
                level,
                filter
        );
    }

    public @Nullable EntityHitResult clipEntityInView(Predicate<Entity> filter){
        if(level == null)return null;
        ClipContext context = clipContext();
        return ClipUtils.clipEntityInView(
                context.getFrom(),
                context.getTo(),
                coneAABB(),
                coneAngle,
                level,
                filter
        );
    }

    public @NotNull List<Entity> clipAllEntityInView(Predicate<Entity> filter){
        if(level == null)return List.of();
        ClipContext context = clipContext();
        return ClipUtils.clipAllEntityInView(
                context.getFrom(),
                context.getTo(),
                coneAABB(),
                0.2,
                level,
                filter
        );
    }


    @OnlyIn(Dist.CLIENT)
    public void outlineViewClip(){
        BlockHitResult hitResult = clipBlock(false);
        if(hitResult == null)return;
        if(level == null)return;
        double sign_x = hitResult.getDirection().getAxis() == Direction.Axis.X ? 0 : 1;
        double sign_y = hitResult.getDirection().getAxis() == Direction.Axis.Y ? 0 : 1;
        double sign_z = hitResult.getDirection().getAxis() == Direction.Axis.Z ? 0 : 1;


        outlineLocation(
                hitResult.getLocation().add(new Vec3(-0.5 * sign_x, -0.5 * sign_y, -0.5 * sign_z)),
                hitResult.getDirection().getOpposite(),
                Color.RED.getRGB(),
                "camera_clip"
        );
    }

    @OnlyIn(Dist.CLIENT)
    public void outlineEntityClip(){
        EntityHitResult hitResult = clipEntity((e) -> true);
        if(hitResult == null)return;
        if(level == null)return;
        if(hitResult.getEntity() instanceof IEntityDuck lv){
            lv.controlCraft$setClientGlowing(4);
        }
    }

    @OnlyIn(Dist.CLIENT)
    public void outlineEntityInView(){
        Optional.ofNullable(clipEntityInView(
                e ->
                        e instanceof LivingEntity lv &&
                        e.isAlive() &&
                        !e.isRemoved() &&
                        lv.getHealth() > 0.1
        )).ifPresent(
                e -> {
                    if(e.getEntity() instanceof IEntityDuck lv){
                        lv.controlCraft$setClientGlowing(4);
                    }
                }
        );
    }

    @OnlyIn(Dist.CLIENT)
    public void outlineAllEntityInView(){
        /*
        */
        clipAllEntityInView(LivingEntity.class::isInstance).forEach(
            e -> {
                if(e instanceof IEntityDuck lv){
                    lv.controlCraft$setClientGlowing(4);
                }
            }
        );
    }


    @OnlyIn(Dist.CLIENT)
    public void db_outlineConeAABB(){
        AABB coneAABB = coneAABB();

        Vector3dc center = getCameraPosition(); // VSGetterUtils.getAbsolutePosition(WorldBlockPos.of(level, worldPosition));
        Vector3dc view = getAbsViewForward();
        Vector3dc camEnd = center.fma(clipRange * 0.7, view, new Vector3d());
        double radiusSquare = Math.tan(coneAngle) * clipRange;
        AABBd endAABB = MathUtils.centerWithRadius(camEnd, radiusSquare).intersection(toJOML(trivialAABB()));

        CreateClient.OUTLINER.showAABB(
                "camera_cone_aabb",
                coneAABB
        )
                .colored(Color.RED)
                .lineWidth(4f);

        CreateClient.OUTLINER.showAABB(
                "camera_cone_aabb_end",
                toMinecraft(endAABB)
        )
                .colored(Color.GREEN)
                .lineWidth(4f);
    }



    @OnlyIn(Dist.CLIENT)
    public void outlineClipRay(){
        ClipContext clipContext = clipContext();
        Vector3d offset = isLinkedCamera() ? new Vector3d(0, -0.5, 0) : new Vector3d(0, 0, 0);

    /*
        ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine(
                "camera_clip_ray_" + getBlockPos().asLong(),
                clipContext.getFrom().add(toMinecraft(portPos)),
                clipContext.getTo()
        )
                .colored(Color.SPRING_GREEN.setAlpha(0.6f))
                .lineWidth(0.3f);
    * */
        Matrix4dc w2c = Optional
                .ofNullable(getClientShip())
                .map(ClientShip::getRenderTransform)
                .map(ShipTransform::getWorldToShip)
                .orElse(new Matrix4d());


        Function<Vec3, Vec3> convert = v -> {
            Vector3d v1 = new Vector3d(v.x, v.y, v.z);
            return toMinecraft(w2c.transformPosition(v1));
        };

        ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine(
                        "camera_clip_ray_" + getBlockPos().asLong(),
                        convert.apply(clipContext.getFrom()).add(toMinecraft(offset)),
                        convert.apply(clipContext.getTo())
                )
                .colored(Color.SPRING_GREEN.setAlpha(0.6f))
                .lineWidth(0.3f);


    }

    @OnlyIn(Dist.CLIENT)
    public void outlineClipRayShip(){
        ClipContext clipContext = clipContextShip();
        Vector3d offset = isLinkedCamera() ? new Vector3d(0, -0.5, 0) : new Vector3d(0, 0, 0);



        ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine(
                        "camera_clip_ray_" + getBlockPos().asLong(),
                        clipContext.getFrom().add(toMinecraft(offset)),
                        clipContext.getTo()
                )
                .colored(Color.SPRING_GREEN.setAlpha(0.6f))
                .lineWidth(0.3f);


    }

    @OnlyIn(Dist.CLIENT)
    public void outlineShipClip(){
        ShipHitResult hitResult = clipShip();
        if(hitResult == null)return;
        if(level == null)return;
        double distance = hitResult.ship().getTransform().getPositionInWorld().distance(getCameraPosition());
        ClientOutliner.drawOutline(
                VectorConversionsMCKt.toMinecraft(hitResult.ship().getWorldAABB()),
                Color.SPRING_GREEN.getRGB(),
                "camera_clip_ship",
                distance / 5
        );
    }

    @OnlyIn(Dist.CLIENT)
    private void outlineLocation(Vec3 center, Direction direction, int color, String slot){
        double distance = center.subtract(ValkyrienSkies.toMinecraft(getCameraPosition())).length();
        double scale = distance / 5;
        ClientOutliner.drawOutline(center, direction, scale, color, slot);
        // ClientOutliner.drawOutline(center, direction.getOpposite(), scale, color, slot + "_opposite");
    }

    @OnlyIn(Dist.CLIENT)
    public void outlineExtra(Vec3 pos, Direction direction, String slot, int rgb){
        if(level == null)return;
        if(!ClientCameraManager.isLinked())return;
        if(!isLinkedCamera())return;
        outlineLocation(pos, direction, rgb, slot);

    }


    public void outlineExtraTo(CameraDrawingContext context){
        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.SYNC_0)
                .withDouble(context.pos.x)
                .withDouble(context.pos.y)
                .withDouble(context.pos.z)
                .withUtf8(context.dir.name())
                .withUtf8(context.slot)
                .withLong(context.rgb)
                .build();

        ControlCraftPackets.sendToPlayer(p, context.player);
    }

    public void outlineExtraToUser(Vec3 pos, Direction dir, String slot, int rgb){
        if(level == null || level.isClientSide)return;
        ServerPlayer player = ((ServerLevel)level)
                .players()
                .stream()
                .filter(p -> p.getUUID().equals(ServerCameraManager.getUserUUID(WorldBlockPos.of(level, getBlockPos()))))
                .findFirst()
                .orElse(null);
        if(player == null)return;
        CameraDrawingContext ctx = new CameraDrawingContext(player, pos, dir, slot, rgb);
        outlineExtraTo(ctx);
    }

    public Vector3d getCameraPositionShip(){
        Vec3 front = getBlockPos().getCenter();
        return new Vector3d(front.x, front.y, front.z);
    }

    public Vector3d getCameraPosition(){
        Vector3d original = getCameraPositionShip();
        if(level == null) return original;
        return Optional
                .ofNullable(getShipOn())
                .map(Ship::getTransform)
                .map(t -> t
                        .getShipToWorld()
                        .transformPosition(original))
                .orElse(original);
    }

    public Quaterniondc getCameraBaseRotation(){
        Quaterniond q = new Quaterniond();
        Ship ship = getShipOn();
        if(ship == null)return q;
        if(ship instanceof ClientShip cs)return cs.getRenderTransform().getShipToWorldRotation();
        return ship.getTransform().getShipToWorldRotation();
    }


    @Override
    public void tickServer() {
        super.tickServer();
        if(isActiveDistanceSensor()){
            //setChanged();
            updateOutputSignal();
            updateNeighbor();
        }
        // getOrCreateFakePlayer()._tick();
        syncForOtherPlayers();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        syncForNear(true, RAY_TYPE, SHIP_TYPE, ENTITY_TYPE , IS_ACTIVE_SENSOR, FIELD);
    }


    @OnlyIn(Dist.CLIENT)
    @Override
    public void tickClient() {
        super.tickClient();

        // db_outlineConeAABB();
        if(
                rayType() == CameraClipType.RAY_ALWAYS
                        ||
                rayType() == CameraClipType.RAY_ON_USE && isLinkedCamera()
        ){
            outlineClipRayShip();
        }

        if(shipType() == CameraClipType.SHIP_CLIP_ON && isLinkedCamera())outlineShipClip();

        if(isLinkedCamera()){
            if(entityType() == CameraClipType.ENTITY_IN_VIEW)outlineAllEntityInView();
            if(entityType() == CameraClipType.ENTITY_NEAREST)outlineEntityInView();
            if(entityType() == CameraClipType.ENTITY_TARGETED_ONLY)outlineEntityClip();
        }

    }

    public Quaterniond getAbsViewTransform(){
        Quaterniondc originalRotation =
                new Quaterniond().rotateY(Math.toRadians(-yaw)).rotateX(Math.toRadians(pitch)).normalize();
        return getCameraBaseRotation().mul(originalRotation, new Quaterniond());
    }

    public Vector3d getAbsViewForward(){
        return getAbsViewTransform().transform(new Vector3d(0, 0, 1));
    }

    public Quaterniond getLocViewTransform(){
        return new Quaterniond().rotateY(Math.toRadians(-yaw)).rotateX(Math.toRadians(pitch)).normalize();
    }

    public Vector3d getLocViewForward(){
        return getLocViewTransform().transform(new Vector3d(0, 0, 1));
    }


    public void syncServer(String uuid){
        if(level == null || !level.isClientSide)return;
        var p = new BlockBoundServerPacket.builder(getBlockPos(), RegisteredPacketType.SETTING_0)
                .withDouble(pitch)
                .withDouble(yaw)
                .withUtf8(uuid)
                .build();
        ControlCraftPackets.getChannel().sendToServer(p);
    }

    public void syncServerNoChunkLoading(String uuid){
        if(level == null || !level.isClientSide)return;
        var p = new BlockBoundServerPacket.builder(getBlockPos(), RegisteredPacketType.SETTING_1)
                .withDouble(pitch)
                .withDouble(yaw)
                .withUtf8(uuid)
                .build();
        ControlCraftPackets.getChannel().sendToServer(p);
    }

    @OnlyIn(Dist.CLIENT)
    public void syncServerFromClient(){
        if(level == null || !level.isClientSide)return;
        Player player = Minecraft.getInstance().player;
        if(player == null)return;
        syncServerNoChunkLoading(player.getName().getString());
    }

    public void syncForOtherPlayers(){
        if(level == null || level.isClientSide)return;
        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.SETTING_1)
                .withDouble(pitch)
                .withDouble(yaw)
                .build();
        ControlCraftPackets.getChannel().send(
                PacketDistributor.ALL.noArg(),
                p
        );
    }

    public void displayScreen(ServerPlayer player){
        var p = new BlockBoundClientPacket.builder(getBlockPos(), RegisteredPacketType.OPEN_SCREEN_0)
                .withBoolean(isActiveDistanceSensor)
                .build();

        ControlCraftPackets.sendToPlayer(p, player);
    }

    @Override
    public void handleServer(NetworkEvent.Context context, BlockBoundServerPacket packet) {
        if(packet.getType() == RegisteredPacketType.SETTING_0){
            double p = packet.getDoubles().get(0);
            double y = packet.getDoubles().get(1);
            String uuid = packet.getUtf8s().get(0);
            setPitch(p);
            setYaw(y);

            ServerPlayer user = context.getSender();
            if(user == null || level == null)return;


            getOrCreateFakePlayer().activate(user);

        }

        if(packet.getType() == RegisteredPacketType.SETTING_1){
            double p = packet.getDoubles().get(0);
            double y = packet.getDoubles().get(1);
            String uuid = packet.getUtf8s().get(0);
            setPitch(p);
            setYaw(y);
        }

        if(packet.getType() == RegisteredPacketType.EXTEND_0){
            ServerPlayer user = context.getSender();
            if(user == null || level == null)return;

            // let the move(ServerPlayer p) send chunks again to user, avoiding holes on client side
            user.setLastSectionPos(tracker.lastSectionPos());

            ServerCameraManager.remove(user.getUUID());  //WorldBlockPos.of(level, getBlockPos())

        }
        if(packet.getType() == RegisteredPacketType.EXTEND_1){
            ServerPlayer user = context.getSender();
            if(user == null || level == null)return;


            ServerCameraManager.registerUser(WorldBlockPos.of(level, getBlockPos()), user);


            var ack = new ReceiveLatestWorldPosPacket(toMinecraft(getCameraPosition()));
            ControlCraftPackets.sendToPlayer(ack, user);

            getOrCreateFakePlayer().activate(user);

            /*
            ControlCraftServer.SERVER_EXECUTOR.executeLater(
                    () -> finalFp,
                    20
            );
            * */

        }
    }

    @OnlyIn(Dist.CLIENT)
    @Override
    public void handleClient(NetworkEvent.Context context, BlockBoundClientPacket packet) {
        if(packet.getType() == RegisteredPacketType.SYNC_0){
            Vec3 pos = new Vec3(
                    packet.getDoubles().get(0),
                    packet.getDoubles().get(1),
                    packet.getDoubles().get(2)
            );
            Direction dir = Direction.valueOf(
                    packet.getUtf8s().get(0)
            );
            String slot =
                    packet.getUtf8s().get(1);
            int rgb =
                    packet.getLongs().get(0).intValue();
            outlineExtra(pos, dir, slot, rgb);
        }
        if(packet.getType() == RegisteredPacketType.SYNC_1){
            double p = packet.getDoubles().get(0);
            double y = packet.getDoubles().get(1);
            setPitchForceClient(p);
            setYawForceClient(y);
            // syncServerFromClient();
        }
        if(packet.getType() == RegisteredPacketType.SETTING_1){
            if(isLinkedCamera())return;
            double p = packet.getDoubles().get(0);
            double y = packet.getDoubles().get(1);
            setPitchForceClient(p);
            setYawForceClient(y);
        }

    }

    @Override
    public void initialize() {
        super.initialize();
        if(level == null || level.isClientSide)return;
        getOrCreateFakePlayer();
    }

    private CameraBoundFakePlayer getOrCreateFakePlayer(){
        if(fp == null) fp = new CameraBoundFakePlayer((ServerLevel) level, this);
        return fp;
    }

    public CameraBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        this.tracker = new CameraMovementTracker(pos);

        buildRegistry(PITCH).withBasic(SerializePort.of(this::getPitch, this::setPitch, SerializeUtils.DOUBLE)).register();
        buildRegistry(YAW).withBasic(SerializePort.of(this::getYaw, this::setYaw, SerializeUtils.DOUBLE)).register();
        buildRegistry(IS_ACTIVE_SENSOR)
                .withBasic(SerializePort.of(
                        this::isActiveDistanceSensor,
                        this::setActiveDistanceSensor,
                        SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        buildRegistry(RAY_TYPE)
                .withBasic(
                    SerializePort.of(
                            this::rayType,
                            this::setRayType,
                            SerializeUtils.ofEnum(CameraClipType.class)
                    )
                )
                .withClient(
                    ClientBuffer.of(CameraClipType.class)
                )
                .dispatchToSync()
                .register();

        buildRegistry(SHIP_TYPE)
                .withBasic(
                        SerializePort.of(
                                this::shipType,
                                this::setShipType,
                                SerializeUtils.ofEnum(CameraClipType.class)
                        )
                )
                .withClient(
                        ClientBuffer.of(CameraClipType.class)
                )
                .dispatchToSync()
                .register();

        buildRegistry(ENTITY_TYPE)
                .withBasic(
                        SerializePort.of(
                                this::entityType,
                                this::setEntityType,
                                SerializeUtils.ofEnum(CameraClipType.class)
                        )
                )
                .withClient(
                        ClientBuffer.of(CameraClipType.class)
                )
                .dispatchToSync()
                .register();

        buildRegistry(FIELD)
                .withBasic(CompoundTagPort.of(
                        () -> receiver().serialize(),
                        t -> receiver().deserialize(t)
                ))
                .withClient(
                        new ClientBuffer<>(SerializeUtils.UNIT, CompoundTag.class)
                )
                .dispatchToSync()
                .register();

        panel().registerUnit(
                RESET,
                this::resetView
        );

        receiver().register(
                new NumericField(
                        () -> 1.0,
                        t -> {},
                        "sensor"
                ),
                new DirectReceiver.InitContext(SlotType.IS_SENSOR, Couple.create(0.0, 1.0))
        );
    }



    @Override
    public String name() {
        return "camera";
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public boolean addToGoggleTooltip(List<Component> tooltip, boolean isPlayerSneaking) {

        Direction dir = MinecraftUtils.lookingAtFaceDirection();

        return true;
    }

    @Override
    public @NotNull NamedComponent plant() {
        return plant;
    }

    public record CameraDrawingContext(ServerPlayer player, Vec3 pos, Direction dir, String slot, int rgb) { }
}

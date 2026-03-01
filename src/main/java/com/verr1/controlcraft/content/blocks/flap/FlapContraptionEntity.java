package com.verr1.controlcraft.content.blocks.flap;

import com.jozufozu.flywheel.util.transform.TransformStack;
import com.mojang.blaze3d.vertex.PoseStack;
import com.simibubi.create.content.contraptions.*;
import com.simibubi.create.foundation.utility.NBTHelper;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.registry.ControlCraftEntities;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.phys.Vec3;
import org.joml.Quaterniond;
import org.joml.Quaternionf;
import org.joml.Vector3d;
import org.joml.Vector3f;

import static com.simibubi.create.foundation.utility.AngleHelper.angleLerp;

public class FlapContraptionEntity extends AbstractContraptionEntity {

    protected BlockPos controllerPos;
    protected Direction angleDirection = Direction.UP;
    protected Direction tiltDirection = Direction.NORTH;

    protected double prevAngle;
    protected double angle;
    protected double prevTilt;
    protected double tilt;

    public FlapContraptionEntity(EntityType<?> entityTypeIn, Level worldIn) {
        super(entityTypeIn, worldIn);
    }

    public static FlapContraptionEntity create(
        Level world,
        CompactFlapBlockEntity controller,
        Contraption contraption
    ) {
        FlapContraptionEntity entity =
            new FlapContraptionEntity(ControlCraftEntities.FLAP.get(), world);
        entity.controllerPos = controller.getBlockPos();
        entity.setContraption(contraption);
        return entity;
    }

    @Override
    protected void readAdditional(CompoundTag compound, boolean spawnPacket) {
        super.readAdditional(compound, spawnPacket);
        if (compound.contains("ControllerRelative")){
            controllerPos = NbtUtils.readBlockPos(compound.getCompound("ControllerRelative"))
                .offset(blockPosition());
        }

        if (compound.contains("AngleDirection"))
            angleDirection = NBTHelper.readEnum(compound, "AngleDirection", Direction.class);

        if (compound.contains("TiltDirection"))
            tiltDirection = NBTHelper.readEnum(compound, "TiltDirection", Direction.class);

        angle = compound.getFloat("Angle");
        tilt = compound.getFloat("Tilt");
    }

    @Override
    protected void writeAdditional(CompoundTag compound, boolean spawnPacket) {
        super.writeAdditional(compound, spawnPacket);
        if(controllerPos != null){
            compound.put("ControllerRelative", NbtUtils.writeBlockPos(controllerPos.subtract(blockPosition())));
        }

        NBTHelper.writeEnum(compound, "AngleDirection", angleDirection);
        NBTHelper.writeEnum(compound, "TiltDirection", tiltDirection);
        compound.putFloat("Angle", (float)angle);
        compound.putFloat("Tilt", (float)tilt);
    }

    @Override
    protected void tickContraption() {
        prevAngle = angle;
        prevTilt = tilt;

        if (controllerPos == null)
            return;
        if (!level().isLoaded(controllerPos))
            return;
        CompactFlapBlockEntity controller = getFlap();
        if (controller == null) {
            discard();
            return;
        }
        if (!controller.isAttachedTo(this)) {
            controller.attach(this);
            if (level().isClientSide)
                setPos(getX(), getY(), getZ());
        }
    }

    protected CompactFlapBlockEntity getFlap(){
        if (controllerPos == null)
            return null;
        if (!level().isLoaded(controllerPos))
            return null;
        BlockEntity be = level().getBlockEntity(controllerPos);
        if (!(be instanceof CompactFlapBlockEntity flap))
            return null;
        return flap;
    }

    public float getAngle(float partialTicks) {
        return partialTicks == 1.0F ? (float) angle : angleLerp(partialTicks, prevAngle, angle);
    }

    public float getTilt(float partialTicks) {
        return partialTicks == 1.0F ? (float) tilt : angleLerp(partialTicks, prevTilt, tilt);
    }

    @Override
    public Vec3 applyRotation(Vec3 localPos, float partialTicks) {
        Vector3d rotateAxis = getRotateAxis();
        Vector3d tiltAxis = getTiltAxis();
        double radians0 = Math.toRadians(MathUtils.angleReset(getAngle(partialTicks)));
        double radians1 = Math.toRadians(MathUtils.angleReset(getTilt(partialTicks)));
        Vector3d applied = ValkyrienSkies.toJOML(localPos)
            .rotateAxis((radians0), rotateAxis.x(), rotateAxis.y(), rotateAxis.z())
            .rotateAxis((radians1), tiltAxis.x(), tiltAxis.y(), tiltAxis.z());
        return ValkyrienSkies.toMinecraft(applied);
    }

    @Override
    public Vec3 reverseRotation(Vec3 localPos, float partialTicks) {
        Vector3d rotateAxis = getRotateAxis();
        Vector3d tiltAxis = getTiltAxis();
        double radians0 = -Math.toRadians(MathUtils.angleReset(angle));
        double radians1 = -Math.toRadians(MathUtils.angleReset(tilt));
        Vector3d applied = ValkyrienSkies.toJOML(localPos)
            .rotateAxis((radians1), tiltAxis.x(), tiltAxis.y(), tiltAxis.z())
            .rotateAxis((radians0), rotateAxis.x(), rotateAxis.y(), rotateAxis.z());
        return ValkyrienSkies.toMinecraft(applied);
    }

    @Override
    protected StructureTransform makeStructureTransform() {
        BlockPos offset = BlockPos.containing(getAnchorVec().add(.5, .5, .5));
        Vector3f xyz = getRotZYX().get(new Vector3f());

        return new StructureTransform(offset, xyz.x, xyz.y, xyz.z);
    }

    public Vector3d getRotZYX(){
        return getRotZYX(0);
    }

    public Quaterniond getRot(float partialTick){
        Vector3d rotateAxis = getRotateAxis();
        Vector3d tiltAxis = getTiltAxis();
        double radians0 = Math.toRadians(MathUtils.angleReset(getAngle(partialTick)));
        double radians1 = Math.toRadians(MathUtils.angleReset(getTilt(partialTick)));

        return new Quaterniond()
            .rotateAxis((radians1), tiltAxis.x(), tiltAxis.y(), tiltAxis.z())
            .rotateAxis((radians0), rotateAxis.x(), rotateAxis.y(), rotateAxis.z());

    }

    public Vector3d getRotZYX(float partialTick){
        return getRot(partialTick)
            .getEulerAnglesZYX(new Vector3d())
            .mul(Math.toDegrees(1));
    }

    @Override
    protected float getStalledAngle() {
        return (float) angle;
    }

    @Override
    protected void handleStallInformation(double x, double y, double z, float angle) {
        setPosRaw(x, y, z);
        this.angle = this.prevAngle = angle;
    }

    @Override
    public ContraptionRotationState getRotationState() {
        ContraptionRotationState crs = new ContraptionRotationState();
        Vector3f xyz = getRotZYX().get(new Vector3f());
        crs.xRotation = xyz.x;
        crs.yRotation = xyz.y;
        crs.zRotation = xyz.z;
        return crs;
    }

    @Override
    public void applyLocalTransforms(PoseStack matrixStack, float partialTicks) {
        TransformStack.cast(matrixStack)
            .nudge(getId())
            .rotateCentered(getRot(partialTicks).get(new Quaternionf()));
    }

    public void setAngle(float v) {
        angle = v;
    }


    public void setTilt(float v) {
        tilt = v;
    }

    public void setAngleDirection(Direction rotationAxis) {
        this.angleDirection = rotationAxis;
    }

    public void setTiltDirection(Direction rotationAxis) {
        this.tiltDirection = rotationAxis;
    }

    private Vector3d getRotateAxis(){
        return ValkyrienSkies.set(new Vector3d(), angleDirection.getNormal());
    }

    private Vector3d getTiltAxis(){
        return ValkyrienSkies.set(new Vector3d(), tiltDirection.getNormal());
    }

    public Direction getAngleDirection() {
        return angleDirection;
    }

    public Direction getTiltDirection() {
        return angleDirection;
    }
}

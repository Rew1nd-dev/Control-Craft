package com.verr1.controlcraft.content.blocks.controlchair;

import com.simibubi.create.content.contraptions.actors.seat.SeatEntity;
import com.simibubi.create.foundation.utility.animation.LerpedFloat;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.cimulink.game.peripheral.ControlChairPlant;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.input.ControlChairAxis;
import com.verr1.controlcraft.foundation.input.ControlChairButton;
import com.verr1.controlcraft.foundation.input.ControlChairClientInput;
import com.verr1.controlcraft.foundation.input.ControlChairInputFrame;
import com.verr1.controlcraft.foundation.input.ControlChairInputManager;
import com.verr1.controlcraft.foundation.input.ControlChairSession;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.type.descriptive.ControlChairRotationMode;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.util.Mth;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniondc;
import org.joml.Quaterniond;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.core.api.ships.Ship;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

public class ControlChairBlockEntity extends OnShipBlockEntity implements IPlant {
    public static final NetworkKey FIRST_PERSON_ROTATION_MODE = NetworkKey.create("first_person_rotation_mode");
    public static final NetworkKey THIRD_PERSON_ROTATION_MODE = NetworkKey.create("third_person_rotation_mode");
    private static final float HANDLE_MAX_ANGLE = 30.0f;
    private static final double HANDLE_INPUT_LIMIT = 1.0;
    private static final double HANDLE_CHASE_SPEED = 0.25;

    private final ControlChairPlant plant;
    private final LerpedFloat animatedHandleRoll = LerpedFloat.angular();
    private final LerpedFloat animatedHandlePitch = LerpedFloat.angular();

    private @Nullable UUID currentPilotId = null;
    private String currentPilotName = "";
    private ControlChairRotationMode firstPersonRotationMode = ControlChairRotationMode.FOLLOW_SHIP;
    private ControlChairRotationMode thirdPersonRotationMode = ControlChairRotationMode.FOLLOW_SHIP;
    private float targetHandleRoll = 0.0f;
    private float targetHandlePitch = 0.0f;

    public ControlChairBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);

        buildRegistry(FIRST_PERSON_ROTATION_MODE)
                .withBasic(SerializePort.of(
                        this::firstPersonRotationMode,
                        this::setFirstPersonRotationMode,
                        SerializeUtils.ofEnum(ControlChairRotationMode.class)
                ))
                .withClient(ClientBuffer.ofEnum(ControlChairRotationMode.class))
                .dispatchToSync()
                .register();

        buildRegistry(THIRD_PERSON_ROTATION_MODE)
                .withBasic(SerializePort.of(
                        this::thirdPersonRotationMode,
                        this::setThirdPersonRotationMode,
                        SerializeUtils.ofEnum(ControlChairRotationMode.class)
                ))
                .withClient(ClientBuffer.ofEnum(ControlChairRotationMode.class))
                .dispatchToSync()
                .register();

        this.plant = new ControlChairPlant(this);
    }

    @Override
    public void tickServer() {
        super.tickServer();
        refreshPilotBinding();
    }

    @Override
    public void tickClient() {
        super.tickClient();
        tickHandleAnimation();
    }

    @Override
    public void removeServer() {
        super.removeServer();
        releaseBinding();
    }

    public boolean isOccupied() {
        return currentPilotId != null;
    }

    public double occupiedSignal() {
        return isOccupied() ? 1.0 : 0.0;
    }

    public boolean acceptingInput() {
        return session().map(ControlChairSession::acceptingInput).orElse(false);
    }

    public double axis(ControlChairAxis axis) {
        return acceptingInput() ? latestFrame().axis(axis) : 0.0;
    }

    public boolean button(ControlChairButton button) {
        return acceptingInput() && latestFrame().held(button);
    }

    public boolean thirdPersonView() {
        return session().map(ControlChairSession::thirdPersonView).orElse(false);
    }

    public double viewYaw() {
        return session().map(ControlChairSession::viewYaw).orElse(0.0);
    }

    public double viewPitch() {
        return session().map(ControlChairSession::viewPitch).orElse(0.0);
    }

    public double absYaw() {
        return angle(absViewForward()).yaw();
    }

    public double absPitch() {
        return angle(absViewForward()).pitch();
    }

    public Vector3d absViewForward() {
        return getAbsViewTransform()
                .transform(new Vector3d(0, 0, 1))
                .normalize();
    }

    public ControlChairRotationMode firstPersonRotationMode() {
        return firstPersonRotationMode;
    }

    public void setFirstPersonRotationMode(ControlChairRotationMode firstPersonRotationMode) {
        this.firstPersonRotationMode = firstPersonRotationMode;
        queueUpdate(FIRST_PERSON_ROTATION_MODE);
    }

    public ControlChairRotationMode thirdPersonRotationMode() {
        return thirdPersonRotationMode;
    }

    public void setThirdPersonRotationMode(ControlChairRotationMode thirdPersonRotationMode) {
        this.thirdPersonRotationMode = thirdPersonRotationMode;
        queueUpdate(THIRD_PERSON_ROTATION_MODE);
    }

    public ControlChairRotationMode rotationMode(boolean thirdPerson) {
        return thirdPerson ? thirdPersonRotationMode() : firstPersonRotationMode();
    }

    public ControlChairRotationMode activeRotationMode() {
        return rotationMode(thirdPersonView());
    }

    public boolean transformRotation(boolean thirdPerson) {
        return rotationMode(thirdPerson) == ControlChairRotationMode.FOLLOW_SHIP;
    }

    public boolean transformRotation() {
        return transformRotation(thirdPersonView());
    }

    @OnlyIn(Dist.CLIENT)
    public float animatedHandleRoll(float partialTicks) {
        return animatedHandleRoll.getValue(partialTicks);
    }

    @OnlyIn(Dist.CLIENT)
    public float animatedHandlePitch(float partialTicks) {
        return animatedHandlePitch.getValue(partialTicks);
    }

    public @Nullable UUID currentPilotId() {
        return currentPilotId;
    }

    public String currentPilotName() {
        return currentPilotName;
    }

    public Optional<ControlChairSession> session() {
        if (currentPilotId == null) {
            return Optional.empty();
        }
        return ControlChairInputManager.sessionOf(currentPilotId);
    }

    private void refreshPilotBinding() {
        Player pilot = findPilot();
        UUID nextPilotId = pilot == null ? null : pilot.getUUID();

        if (Objects.equals(currentPilotId, nextPilotId)) {
            if (pilot != null) {
                currentPilotName = pilot.getName().getString();
                ControlChairInputManager.ensureBound(pilot.getUUID(), getWorldBlockPos());
            }
            if (pilot == null) {
                currentPilotName = "";
            }
            return;
        }

        releaseBinding();
        currentPilotId = nextPilotId;
        currentPilotName = pilot == null ? "" : pilot.getName().getString();

        if (nextPilotId != null) {
            ControlChairInputManager.bind(nextPilotId, getWorldBlockPos());
        }
    }

    private void releaseBinding() {
        if (currentPilotId != null) {
            ControlChairInputManager.releasePlayer(currentPilotId);
        } else {
            ControlChairInputManager.releaseChair(getWorldBlockPos());
        }
        currentPilotId = null;
        currentPilotName = "";
    }

    private @Nullable Player findPilot() {
        if (level == null) {
            return null;
        }

        return level.getEntitiesOfClass(SeatEntity.class, new AABB(getBlockPos()))
                .stream()
                .flatMap(seat -> seat.getPassengers().stream())
                .filter(Player.class::isInstance)
                .map(Player.class::cast)
                .findFirst()
                .orElse(null);
    }

    @Override
    public @NotNull NamedComponent plant() {
        return plant;
    }

    public Quaterniond getViewTransform() {
        return new Quaterniond()
                .rotateY(Math.toRadians(-viewYaw()))
                .rotateX(Math.toRadians(viewPitch()))
                .normalize();
    }

    public Quaterniond getAbsViewTransform() {
        Quaterniondc rawRotation = getViewTransform();
        return transformRotation()
                ? getViewBaseRotation().mul(rawRotation, new Quaterniond())
                : new Quaterniond(rawRotation);
    }

    public Quaterniond getViewBaseRotation() {
        Quaterniond q = new Quaterniond();
        Ship ship = getShipOn();
        if (ship == null) {
            return q;
        }
        if (ship instanceof ClientShip clientShip) {
            return new Quaterniond(clientShip.getRenderTransform().getShipToWorldRotation());
        }
        return new Quaterniond(readSelf().quaternion());
    }

    private ControlChairInputFrame latestFrame() {
        return session().map(ControlChairSession::latestFrame).orElse(ControlChairInputFrame.empty());
    }

    @OnlyIn(Dist.CLIENT)
    private void tickHandleAnimation() {
        updateTargetHandleAngles();
        animatedHandleRoll.chase(targetHandleRoll, HANDLE_CHASE_SPEED, LerpedFloat.Chaser.EXP);
        animatedHandlePitch.chase(targetHandlePitch, HANDLE_CHASE_SPEED, LerpedFloat.Chaser.EXP);
        animatedHandleRoll.tickChaser();
        animatedHandlePitch.tickChaser();
    }

    @OnlyIn(Dist.CLIENT)
    private void updateTargetHandleAngles() {
        ControlChairClientInput.RenderState renderState = ControlChairClientInput.renderState();
        if (!renderState.active() || renderState.chairPos() == null || !getWorldBlockPos().equals(renderState.chairPos())) {
            targetHandleRoll = 0.0f;
            targetHandlePitch = 0.0f;
            return;
        }

        targetHandleRoll = inputToHandleAngle(renderState.roll());
        targetHandlePitch = inputToHandleAngle(renderState.pitch());
    }

    private static float inputToHandleAngle(double input) {
        return (float) Mth.clamp(input, -HANDLE_INPUT_LIMIT, HANDLE_INPUT_LIMIT) * HANDLE_MAX_ANGLE;
    }

    private static Angles angle(Vector3dc view) {
        return new Angles(
                MathUtils.angleReset(Math.toDegrees(Math.atan2(-view.x(), view.z()))),
                Mth.clamp(Math.toDegrees(Math.asin(-view.y())), -90.0, 90.0)
        );
    }

    private record Angles(double yaw, double pitch) {
    }
}

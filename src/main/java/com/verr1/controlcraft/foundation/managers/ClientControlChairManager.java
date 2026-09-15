package com.verr1.controlcraft.foundation.managers;

import com.simibubi.create.content.contraptions.actors.seat.SeatEntity;
import com.verr1.controlcraft.content.blocks.controlchair.ControlChairBlock;
import com.verr1.controlcraft.content.blocks.controlchair.ControlChairBlockEntity;
import com.verr1.controlcraft.content.gui.screens.ControlChairBindingsScreen;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.input.ControlChairClientInput;
import com.verr1.controlcraft.mixin.accessor.MouseHandlerAccessor;
import com.verr1.controlcraft.registry.ControlCraftAllKeyBindings;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.VSAccessUtils;
import net.minecraft.ChatFormatting;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.CameraType;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.Component;
import net.minecraft.util.Mth;
import net.minecraft.world.entity.Entity;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniond;
import org.joml.Vector3d;
import org.valkyrienskies.core.api.ships.ClientShip;
import org.valkyrienskies.mod.common.entity.ShipMountedToData;

import java.util.Objects;
import java.util.Optional;

@OnlyIn(Dist.CLIENT)
public final class ClientControlChairManager {
    private static volatile Snapshot SNAPSHOT = Snapshot.inactive();

    private static @Nullable WorldBlockPos activeChairPos = null;
    private static boolean inputCaptured = false;
    private static boolean viewLockActive = false;
    private static float lockedYaw = 0.0F;
    private static float lockedPitch = 0.0F;
    private static float latestViewYaw = 0.0F;
    private static float latestViewPitch = 0.0F;
    private static float latestWorldYaw = 0.0F;
    private static float latestWorldPitch = 0.0F;
    private static boolean hasActiveChair = false;
    private static boolean lastEffectiveAbsolute = false;

    private ClientControlChairManager() {}

    public static void tick() {
        sampleViewState();

        Minecraft minecraft = Minecraft.getInstance();
        LocalPlayer player = minecraft.player;
        WorldBlockPos nextChairPos = currentChairPos(player);
        boolean chairChanged = !Objects.equals(activeChairPos, nextChairPos);

        if (chairChanged) {
            resetAllInputStates();
            activeChairPos = nextChairPos;
            inputCaptured = false;
            viewLockActive = false;
            captureCurrentViewAsLocked();
        }

        if (activeChairPos != null) {
            while (ControlCraftAllKeyBindings.TOGGLE_CONTROL_CHAIR_INPUT.get().consumeClick()) {
                setInputCaptured(!inputCaptured);
            }
            while (ControlCraftAllKeyBindings.TOGGLE_CONTROL_CHAIR_VIEW_LOCK.get().consumeClick()) {
                toggleViewLockFromInput();
            }
            while (ControlCraftAllKeyBindings.RESET_CONTROL_CHAIR_VIEW.get().consumeClick()) {
                resetViewToChairFacingFromInput();
            }
            while (ControlCraftAllKeyBindings.OPEN_CONTROL_CHAIR_BINDINGS.get().consumeClick()) {
                openBindingsScreenFromInput();
            }
        } else {
            drainChairClicks();
        }

        boolean nextHasActiveChair = activeChairPos != null;
        if (nextHasActiveChair && viewLockActive && !supportsViewLock()) {
            viewLockActive = false;
        }
        if (nextHasActiveChair && viewLockActive) {
            applyLockedView();
        }
        boolean effectiveAbsolute = nextHasActiveChair && useAbsoluteMountedView();
        if (nextHasActiveChair && (chairChanged || lastEffectiveAbsolute != effectiveAbsolute)) {
            reconcilePlayerRotation(effectiveAbsolute);
        }

        hasActiveChair = nextHasActiveChair;
        lastEffectiveAbsolute = effectiveAbsolute;
        publishSnapshot();
    }

    public static void captureViewFromMainCamera() {
        sampleViewState();
        publishSnapshot();
    }

    public static boolean hasActiveChair() {
        return activeChairPos != null;
    }

    public static boolean isInputCaptured() {
        return hasActiveChair() && inputCaptured;
    }

    public static boolean isViewLockActive() {
        return hasActiveChair() && viewLockActive;
    }

    public static boolean shouldCancelMouseTurn() {
        return isViewLockActive();
    }

    public static boolean shouldOverrideMountedCamera() {
        return hasActiveChair() && (useAbsoluteMountedView() || isThirdPersonCamera());
    }

    public static Snapshot snapshot() {
        return SNAPSHOT;
    }

    public static @Nullable ShipMountedToData mountedData(float partialTicks) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null || !hasActiveChair()) {
            return null;
        }
        return VSAccessUtils.getShipMountedToData(player, partialTicks);
    }

    public static boolean isReservedKey(int key, int scanCode) {
        return ControlCraftAllKeyBindings.TOGGLE_CONTROL_CHAIR_INPUT.get().matches(key, scanCode)
                || ControlCraftAllKeyBindings.TOGGLE_CONTROL_CHAIR_VIEW_LOCK.get().matches(key, scanCode)
                || ControlCraftAllKeyBindings.RESET_CONTROL_CHAIR_VIEW.get().matches(key, scanCode)
                || ControlCraftAllKeyBindings.OPEN_CONTROL_CHAIR_BINDINGS.get().matches(key, scanCode);
    }

    public static void applyLockedView() {
        if (!isViewLockActive()) {
            return;
        }
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null) {
            return;
        }
        setPlayerView(player, lockedPitch, lockedYaw);
    }

    public static @Nullable ControlChairBlockEntity getActiveChairBlockEntity() {
        Minecraft minecraft = Minecraft.getInstance();
        if (activeChairPos == null || minecraft.level == null) {
            return null;
        }
        if (!(minecraft.level.getBlockEntity(activeChairPos.pos()) instanceof ControlChairBlockEntity chair)) {
            return null;
        }
        return chair;
    }

    private static void setInputCaptured(boolean inputCaptured) {
        resetAllInputStates();
        ClientControlChairManager.inputCaptured = inputCaptured;
        showActionBar(
                Component.literal("Control Chair Input: ")
                        .append(Component.literal(inputCaptured ? "Captured" : "Vanilla")
                                .withStyle(style -> style.withColor(inputCaptured ? ChatFormatting.GREEN : ChatFormatting.YELLOW)))
        );
    }

    private static void setViewLockActive(boolean viewLockActive) {
        if (ClientControlChairManager.viewLockActive == viewLockActive) {
            return;
        }
        ClientControlChairManager.viewLockActive = viewLockActive;
        if (viewLockActive) {
            captureCurrentViewAsLocked();
            applyLockedView();
        }
        showActionBar(
                Component.literal("Control Chair View Lock: ")
                        .append(Component.literal(viewLockActive ? "Locked" : "Free Look")
                                .withStyle(style -> style.withColor(viewLockActive ? ChatFormatting.AQUA : ChatFormatting.YELLOW)))
        );
    }

    private static void toggleViewLockFromInput() {
        if (!supportsViewLock()) {
            if (viewLockActive) {
                viewLockActive = false;
            }
            showActionBar(
                    Component.literal("Control Chair View Lock: ")
                            .append(Component.literal("Unavailable In Absolute View")
                                    .withStyle(style -> style.withColor(ChatFormatting.RED)))
            );
            return;
        }
        setViewLockActive(!viewLockActive);
    }

    private static void resetViewToChairFacingFromInput() {
        if (!supportsViewLock()) {
            showActionBar(
                    Component.literal("Control Chair View Reset: ")
                            .append(Component.literal("Unavailable In Absolute View")
                                    .withStyle(style -> style.withColor(ChatFormatting.RED)))
            );
            return;
        }

        Angles chairFacing = chairFacingAngles();
        if (chairFacing == null) {
            return;
        }

        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null) {
            return;
        }

        setPlayerView(player, chairFacing.pitch(), chairFacing.yaw());
        latestViewYaw = chairFacing.yaw();
        latestViewPitch = chairFacing.pitch();

        if (viewLockActive) {
            lockedYaw = chairFacing.yaw();
            lockedPitch = chairFacing.pitch();
        }

        showActionBar(
                Component.literal("Control Chair View: ")
                        .append(Component.literal("Reset To Chair Facing")
                                .withStyle(style -> style.withColor(ChatFormatting.AQUA)))
        );
    }

    private static void openBindingsScreenFromInput() {
        Minecraft minecraft = Minecraft.getInstance();
        if (minecraft.screen != null) {
            return;
        }

        resetAllInputStates();
        minecraft.setScreen(new ControlChairBindingsScreen());
    }

    private static void drainChairClicks() {
        while (ControlCraftAllKeyBindings.TOGGLE_CONTROL_CHAIR_INPUT.get().consumeClick()) {
        }
        while (ControlCraftAllKeyBindings.TOGGLE_CONTROL_CHAIR_VIEW_LOCK.get().consumeClick()) {
        }
        while (ControlCraftAllKeyBindings.RESET_CONTROL_CHAIR_VIEW.get().consumeClick()) {
        }
        while (ControlCraftAllKeyBindings.OPEN_CONTROL_CHAIR_BINDINGS.get().consumeClick()) {
        }
    }

    private static void publishSnapshot() {
        Minecraft minecraft = Minecraft.getInstance();
        boolean activeForSender = hasActiveChair
                && minecraft.player != null
                && minecraft.level != null
                && minecraft.getConnection() != null
                && minecraft.screen == null
                && minecraft.isWindowActive()
                && !minecraft.isPaused();

        if (!hasActiveChair) {
            SNAPSHOT = Snapshot.inactive();
            return;
        }

        SNAPSHOT = new Snapshot(
                activeForSender,
                activeChairPos,
                inputCaptured,
                viewLockActive,
                latestViewYaw,
                latestViewPitch,
                isServerThirdPersonView(),
                computeMouseRateScale(minecraft),
                minecraft.options.invertYMouse().get()
        );
    }

    private static void sampleViewState() {
        Minecraft minecraft = Minecraft.getInstance();
        LocalPlayer player = minecraft.player;
        if (player == null || minecraft.level == null) {
            return;
        }

        latestViewYaw = player.getYRot();
        latestViewPitch = Mth.clamp(player.getXRot(), -90.0F, 90.0F);
        var lookVector = minecraft.gameRenderer.getMainCamera().getLookVector();
        latestWorldYaw = (float) MathUtils.angleReset(Math.toDegrees(Math.atan2(-lookVector.x, lookVector.z)));
        latestWorldPitch = (float) Mth.clamp(Math.toDegrees(Math.asin(-lookVector.y)), -90.0, 90.0);
    }

    private static void captureCurrentViewAsLocked() {
        lockedYaw = latestViewYaw;
        lockedPitch = latestViewPitch;
    }

    private static @Nullable Angles chairFacingAngles() {
        ControlChairBlockEntity chair = getActiveChairBlockEntity();
        if (chair == null) {
            return null;
        }

        Vector3d forward = chair.transformRotation(isThirdPersonCamera()) ? chair.frontLocal() : chair.front();
        if (forward.lengthSquared() < 1.0E-6) {
            return null;
        }

        return anglesFromForward(forward.normalize());
    }

    private static boolean useAbsoluteMountedView() {
        ControlChairBlockEntity activeChair = getActiveChairBlockEntity();
        if (activeChair == null) {
            return false;
        }
        return !activeChair.transformRotation(isThirdPersonCamera());
    }

    private static boolean supportsViewLock() {
        ControlChairBlockEntity activeChair = getActiveChairBlockEntity();
        if (activeChair == null) {
            return false;
        }
        return activeChair.transformRotation(isThirdPersonCamera());
    }

    private static boolean isThirdPersonCamera() {
        CameraType cameraType = Minecraft.getInstance().options.getCameraType();
        return !cameraType.isFirstPerson();
    }

    private static boolean isServerThirdPersonView() {
        return Minecraft.getInstance().options.getCameraType() == CameraType.THIRD_PERSON_BACK;
    }

    private static void reconcilePlayerRotation(boolean absoluteView) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player == null) {
            return;
        }

        if (viewLockActive) {
            setPlayerView(player, lockedPitch, lockedYaw);
            return;
        }

        if (absoluteView) {
            setPlayerView(player, latestWorldPitch, latestWorldYaw);
            latestViewYaw = latestWorldYaw;
            latestViewPitch = latestWorldPitch;
            return;
        }

        ShipMountedToData mountedData = mountedData(1.0F);
        if (mountedData == null) {
            setPlayerView(player, latestWorldPitch, latestWorldYaw);
            latestViewYaw = latestWorldYaw;
            latestViewPitch = latestWorldPitch;
            return;
        }

        Quaterniond shipRotation = Optional.ofNullable(mountedData.getShipMountedTo())
                .filter(ClientShip.class::isInstance)
                .map(ClientShip.class::cast)
                .map(ClientShip::getRenderTransform)
                .map(transform -> new Quaterniond(transform.getShipToWorldRotation()))
                .orElseGet(Quaterniond::new);

        Vector3d localForward = shipRotation
                .conjugate(new Quaterniond())
                .transform(forwardFromAngles(latestWorldYaw, latestWorldPitch), new Vector3d())
                .normalize();

        Angles localAngles = anglesFromForward(localForward);
        setPlayerView(player, localAngles.pitch(), localAngles.yaw());
        latestViewYaw = localAngles.yaw();
        latestViewPitch = localAngles.pitch();
    }

    private static void setPlayerView(LocalPlayer player, float pitch, float yaw) {
        player.setXRot(pitch);
        player.setYRot(yaw);
        player.xRotO = pitch;
        player.yRotO = yaw;
        player.yHeadRot = yaw;
        player.yHeadRotO = yaw;
        player.yBodyRot = yaw;
        player.yBodyRotO = yaw;
        player.xBob = pitch;
        player.yBob = yaw;
        player.xBobO = pitch;
        player.yBobO = yaw;
    }

    private static Vector3d forwardFromAngles(float yaw, float pitch) {
        return new Quaterniond()
                .rotateY(Math.toRadians(-yaw))
                .rotateX(Math.toRadians(pitch))
                .transform(new Vector3d(0, 0, 1))
                .normalize();
    }

    private static Angles anglesFromForward(Vector3d forward) {
        float yaw = (float) MathUtils.angleReset(Math.toDegrees(Math.atan2(-forward.x(), forward.z())));
        float pitch = (float) Mth.clamp(Math.toDegrees(Math.asin(-forward.y())), -90.0, 90.0);
        return new Angles(yaw, pitch);
    }

    private static double computeMouseRateScale(Minecraft minecraft) {
        double d4 = minecraft.options.sensitivity().get() * 0.6D + 0.2D;
        double d5 = d4 * d4 * d4;
        double sensitivityScale = d5 * 8.0D;

        if (minecraft.player != null
                && minecraft.options.getCameraType().isFirstPerson()
                && minecraft.player.isScoping()) {
            sensitivityScale = d5;
        }

        return sensitivityScale;
    }

    private static void resetAllInputStates() {
        ControlChairClientInput.clearCapturedInputState();
        Minecraft minecraft = Minecraft.getInstance();
        KeyMapping.releaseAll();
        KeyMapping.resetToggleKeys();
        if (minecraft.mouseHandler instanceof MouseHandlerAccessor accessor) {
            accessor.controlCraft$setLeftPressed(false);
            accessor.controlCraft$setMiddlePressed(false);
            accessor.controlCraft$setRightPressed(false);
        }
    }

    private static void showActionBar(Component message) {
        LocalPlayer player = Minecraft.getInstance().player;
        if (player != null) {
            player.displayClientMessage(message, true);
        }
    }

    private static @Nullable WorldBlockPos currentChairPos(@Nullable LocalPlayer player) {
        if (player == null) {
            return null;
        }

        Entity vehicle = player.getVehicle();
        if (!(vehicle instanceof SeatEntity)) {
            return null;
        }

        if (!(player.level().getBlockState(vehicle.blockPosition()).getBlock() instanceof ControlChairBlock)) {
            return null;
        }

        return WorldBlockPos.of(player.level(), vehicle.blockPosition());
    }

    public record Snapshot(
            boolean active,
            @Nullable WorldBlockPos chairPos,
            boolean acceptingInput,
            boolean viewLockActive,
            float viewYaw,
            float viewPitch,
            boolean thirdPersonView,
            double mouseSensitivityScale,
            boolean invertY
    ) {
        private static Snapshot inactive() {
            return new Snapshot(false, null, false, false, 0.0F, 0.0F, false, 0.0D, false);
        }
    }

    private record Angles(float yaw, float pitch) {
    }
}

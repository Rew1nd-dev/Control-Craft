package com.verr1.controlcraft.content.compact.tweak;

public final class TweakMouseLockState {

    private static boolean active = false;
    private static float lockedPitch = 0.0F;
    private static float lockedYaw = 0.0F;

    private TweakMouseLockState() {}

    public static void activate(float pitch, float yaw) {
        active = true;
        lockedPitch = pitch;
        lockedYaw = yaw;
    }

    public static void deactivate() {
        active = false;
    }

    public static boolean isActive() {
        return active;
    }

    public static float lockedPitch() {
        return lockedPitch;
    }

    public static float lockedYaw() {
        return lockedYaw;
    }
}
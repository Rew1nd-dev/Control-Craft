package com.verr1.controlcraft.content.links.computer.lua;

import com.verr1.controlcraft.content.links.computer.ComputerDisplayMetrics;
import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;
import org.joml.Vector3dc;

public interface IComputerCommonContext {

    IPhysAccess getPhysAccess();

    IWorldAccess getWorldAccess();

    Vector3dc frontLocal();

    Vector3dc front();

    Vector3dc leftLocal();

    Vector3dc left();

    Vector3dc upLocal();

    Vector3dc up();

    ComputerDisplayMetrics displayMetrics();

    default int pixelWidth() {
        return displayMetrics().pixelWidth();
    }

    default int pixelHeight() {
        return displayMetrics().pixelHeight();
    }

    default float surfaceWidth() {
        return displayMetrics().surfaceWidth();
    }

    default float surfaceHeight() {
        return displayMetrics().surfaceHeight();
    }

    @Deprecated(forRemoval = false)
    default int width() {
        return pixelWidth();
    }

    @Deprecated(forRemoval = false)
    default int height() {
        return pixelHeight();
    }

}

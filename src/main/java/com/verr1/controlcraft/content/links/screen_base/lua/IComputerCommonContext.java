package com.verr1.controlcraft.content.links.screen_base.lua;

import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import com.verr1.controlcraft.foundation.cimulink.core.api.IWorldAccess;

public interface IComputerCommonContext {

    IPhysAccess getPhysAccess();

    IWorldAccess getWorldAccess();

    int width();

    int height();

}

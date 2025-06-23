package com.verr1.controlcraft.content.compact.createbigcannons;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import dan200.computercraft.api.peripheral.IPeripheral;
import net.minecraft.core.BlockPos;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.fml.ModList;
import org.jetbrains.annotations.Nullable;

public class CreateBigCannonsCompact {

    static ICannonMountPeripheralGetter getter;

    public static @Nullable IPeripheral cannonMountPeripheral(ServerLevel level, BlockPos pos) {
        if (getter == null) return null;
        return getter.get(level, pos);
    }

    public static void init() {
        if(!ModList.get().isLoaded("createbigcannons"))return;

        try {
            Class<?> clazz = Class.forName("com.verr1.controlcraft.content.compact.createbigcannons.impl.CannonMountPeripheralGetter");
            getter = (ICannonMountPeripheralGetter) clazz.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException("Failed to initialize CreateBigCannonsCompact", e);
        }
    }

}

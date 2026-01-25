package com.verr1.controlcraft.mixin.ai;


import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.hooks.VsChunkWatcherHook;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.valkyrienskies.core.apigame.world.IPlayer;
import org.valkyrienskies.core.impl.game.ships.ShipObjectServerWorld;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

// Credits：
// Taken and modified from VSChunkLoaderMod：
// https://github.com/LiterMC/VSChunkLoader/blob/1.20.1/common/src/main/java/com/github/litermc/vschunkloader/mixin/MixinShipObjectServerWorld.java

@Mixin(ShipObjectServerWorld.class)
public class MixinShipObjectServerWorld {


    @ModifyVariable(method = "setPlayers", at = @At("HEAD"), remap = false, argsOnly = true)
    public Set<? extends IPlayer> setPlayers(final Set<? extends IPlayer> players) {
        Set<IPlayer> ps = ServerCameraManager.getAllWatchers();
        // Set<IPlayer> p1 = AiBoundFakePlayer.getAllWatchers();
        final HashSet<IPlayer> playerSet = new HashSet<>(players);
        playerSet.addAll(ps);
        VsChunkWatcherHook.addExtraWatchers(playerSet);
        // playerSet.addAll(p1);
        return playerSet;
    }

}

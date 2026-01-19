package com.verr1.controlcraft.mixin;

import com.verr1.controlcraft.foundation.managers.ServerCameraManager;
import com.verr1.controlcraft.unstable.blocks.AiBoundFakePlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.ModifyVariable;
import org.valkyrienskies.core.impl.shadow.Er;
import org.valkyrienskies.core.internal.world.VsiPlayer;

import java.util.HashSet;
import java.util.Set;

@Mixin(Er.class)
public class MixinVsiServerWorld {

    @ModifyVariable(method = "setPlayers", at = @At("HEAD"), remap = false, argsOnly = true)
    Set<VsiPlayer> addWatchers(Set<VsiPlayer> players){
        Set<VsiPlayer> ps = ServerCameraManager.getAllWatchers();
        Set<VsiPlayer> p1 = AiBoundFakePlayer.getAllWatchers();
        final HashSet<VsiPlayer> playerSet = new HashSet<>(players);
        playerSet.addAll(ps);
        playerSet.addAll(p1);
        return playerSet;
    }

}

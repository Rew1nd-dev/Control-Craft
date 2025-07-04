package com.verr1.controlcraft.content.compact.createbigcannons.impl;

import com.verr1.controlcraft.content.cctweaked.peripheral.AbstractAttachedPeripheral;
import com.verr1.controlcraft.content.cctweaked.peripheral.FlapBearingPeripheral;
import com.verr1.controlcraft.mixinducks.ICannonDuck;
import dan200.computercraft.api.lua.LuaFunction;
import dan200.computercraft.api.peripheral.IPeripheral;
import org.jetbrains.annotations.Nullable;
import rbasamoyai.createbigcannons.cannon_control.cannon_mount.CannonMountBlockEntity;

import java.util.Optional;

public class CannonMountPeripheral extends AbstractAttachedPeripheral<CannonMountBlockEntity> {

    private final ICannonDuck duck;

    public CannonMountPeripheral(CannonMountBlockEntity target) {
        super(target);
        duck = Optional
                .of(target)
                .filter(ICannonDuck.class::isInstance)
                .map(ICannonDuck.class::cast)
                .orElse(null);
    }

    @Override
    public String getType() {
        return "controlcraft$cannon_mount";
    }

    @Override
    public boolean equals(@Nullable IPeripheral iPeripheral) {
        if (!(iPeripheral instanceof CannonMountPeripheral p))return false;
        return getTarget().getBlockPos().equals(p.getTarget().getBlockPos());
    }

    @LuaFunction
    public final void setPitch(double pitch){
        Optional.ofNullable(duck).ifPresent(d -> d.controlCraft$setPitch((float)pitch));
    }

    @LuaFunction
    public final void setYaw(double yaw){
        Optional.ofNullable(duck).ifPresent(d -> d.controlCraft$setYaw((float)yaw));
    }

    @LuaFunction
    public final double getPitch() {
        return  Optional.ofNullable(duck).map(ICannonDuck::controlCraft$getPitch).orElse(0.0f);
    }

    @LuaFunction
    public final double getYaw() {
        return Optional.ofNullable(duck).map(ICannonDuck::controlCraft$getYaw).orElse(0.0f);
    }

    @LuaFunction(mainThread = true)
    public final void assemble(){
        Optional.ofNullable(duck).ifPresent(ICannonDuck::controlCraft$assemble);
    }

    @LuaFunction(mainThread = true)
    public final void disassemble(){
        Optional.ofNullable(duck).ifPresent(ICannonDuck::controlCraft$disassemble);
    }

}

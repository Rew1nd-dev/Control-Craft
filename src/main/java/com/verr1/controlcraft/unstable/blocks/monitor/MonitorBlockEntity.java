package com.verr1.controlcraft.unstable.blocks.monitor;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.blocks.receiver.PeripheralInterfaceBlockEntity;
import com.verr1.controlcraft.content.blocks.spinalyzer.SpinalyzerBlockEntity;
import com.verr1.controlcraft.content.cctweaked.peripheral.SpinalyzerPeripheral;
import com.verr1.controlcraft.content.compact.createbigcannons.APAutocannonAccess;
import com.verr1.controlcraft.content.compact.createbigcannons.CreateBigCannonsCompact;
import com.verr1.controlcraft.content.valkyrienskies.attachments.Observer;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.game.IPlant;
import com.verr1.controlcraft.foundation.data.ExpirableListener;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IFighterJetContext;
import com.verr1.controlcraft.unstable.ai.compact.links.MonitorPlant;
import com.verr1.controlcraft.unstable.ai.core.Address;
import com.verr1.controlcraft.unstable.ai.core.BehaviorTree;
import com.verr1.controlcraft.unstable.ai.core.Blackboard;
import com.verr1.controlcraft.unstable.ai.core.nodes.*;
import com.verr1.controlcraft.unstable.ai.game.PivotAwayAction;
import com.verr1.controlcraft.unstable.ai.game.PivotToAction;
import com.verr1.controlcraft.unstable.ai.game.PivotUpAction;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.ai.game.cruiser.*;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.EvadeExitCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.FighterPullEnterCondition;
import com.verr1.controlcraft.unstable.ai.game.cruiser.conditions.FighterPullExitCondition;
import com.verr1.controlcraft.unstable.blocks.AIBaseBlockEntity;
import com.verr1.controlcraft.unstable.data.schematic.AISchematic;
import com.verr1.controlcraft.unstable.valkyrienskies.context.CruiseMonitor;
import com.verr1.controlcraft.utils.MinecraftUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class MonitorBlockEntity extends AIBaseBlockEntity
{


    private boolean isDead = false;
    private double ratio = 0.9;

    public MonitorBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        registerDouble(this::ratio, this::setRatio, SharedAIKeys.RATIO);
        registerBoolean(this::isDead, this::setIsDead, SharedAIKeys.DEAD);
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickHealth();
    }

    public double ratio() {
        return ratio;
    }

    public void setRatio(double ratio) {
        this.ratio = ratio;
    }

    public boolean isDead() {
        return isDead;
    }

    public void setIsDead(boolean isDead) {
        this.isDead = isDead;
    }

    public double originalMass(){
        long id = getShipOrGroundID();
        return AIServer.MANAGER
                .getDataOf(id)
                .map(d -> d.key)
                .map(AIServer.SCHEMATICS_MANAGER::getLoaded)
                .map(AISchematic::mass)
                .orElse(-1.0);
    }

    public double currentMass(){
        ServerShip ship = getLoadedServerShip();
        if(ship == null)return 0;
        return ship.getInertiaData().getMass();
    }

    public void tickHealth(){
        if(isDead)return;
        ServerShip ship = getLoadedServerShip();
        if(ship == null)return;
        if(!isAI())return;
        double mass = originalMass();
        if(mass < 0){
            ControlCraft.LOGGER.info("missing mass info");
            return;
        }
        double currentMass = currentMass();

        if(currentMass / mass > ratio)return;
        scheduleDeath();
    }

    public void scheduleDeath(){
        if(isDead)return;
        isDead = true;
        MinecraftUtils.broadcastMessage(getShipOrGroundID() + " discarded with " + currentMass() + " original: " + originalMass() + " ratio: " + ratio);
        discard();
        setChanged();
    }

    @Override
    public void onDiscard() {
        // MinecraftUtils.broadcastMessage(getShipOrGroundID() + " discarded with " + readSelf().mass() + " original: " + originalMass());
        // ControlCraft.LOGGER.info("{} discarded with {}, {}", getShipOrGroundID(), readSelf().mass(), originalMass());
    }


    @Override
    public void onSpawn() {
        isDead = false;
        MinecraftUtils.broadcastMessage("monitor spawned");
        setChanged();
    }
}

package com.verr1.controlcraft.unstable.blocks.deploy;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.data.control.ImmutablePhysPose;
import com.verr1.controlcraft.foundation.data.control.ImmutableVel;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.unstable.management.AIPool;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.valkyrienskies.core.api.ships.ServerShip;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Stream;

public abstract class AbstractDeployerBlockEntity extends OnShipBlockEntity {

    protected Set<Long> aliveShips = new HashSet<>();

    public static Optional<ServerShip> get(Long id){
        return pool().getShipOf(id);
    }

    public AbstractDeployerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    protected static AIPool pool(){
        return Objects.requireNonNull(AIServer.MANAGER);
    }

    protected abstract SchematicKey nextType();

    protected abstract ImmutablePhysPose nextPose();

    protected abstract ImmutableVel nextVel();

    protected abstract boolean shouldDeploy();

    protected abstract void onDeploy(@NotNull ServerShip ship, @Nullable AIBlockNetwork network, @NotNull SchematicKey type);

    protected void onDeployFailure(@NotNull SchematicKey type){};

    protected abstract void onDiscard(@NotNull ServerShip ship, @Nullable AIBlockNetwork network, @NotNull  SchematicKey type);

    protected Stream<ServerShip> streamAlive(){
        return aliveShips.stream()
                .map(pool()::getShipOf)
                .map(opt -> opt.orElse(null))
                .filter(Objects::nonNull);
    }

    protected void tickDeploy(){
        onTickStart();
        if(shouldDeploy()){
            var type = nextType();
            var pose = nextPose();
            var vel  = nextVel();
            var ship = pool().spawn(type, pose.pos(), pose.rot(), vel.velocity(), vel.angularVelocity());
            ServerShip serverShip = get(ship.id).orElse(null);
            if(ship.id == -1L || serverShip == null){
                onDeployFailure(type);
                return;
            }
            var network = pool().getNetworkOf(ship.id);

            aliveShips.add(ship.id);
            onDeploy(serverShip, network, type);
        }
    }

    protected void onTickStart(){}

    protected void removeNull(){
        aliveShips.removeIf(id -> pool().getShipOf(id).isEmpty());
    }

    protected void validateHandles(){
       streamAlive()
                .filter(ship -> pool().isInPool(ship.getId()))
                .toList()
                .forEach(
                    ship -> {
                        onDiscard(ship, pool().getNetworkOf(ship.getId()), pool().getTypeOf(ship.getId()));
                        aliveShips.remove(ship.getId());
                    }
        );
       removeNull();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        validateHandles();
    }

    @Override
    public void tickServer() {
        super.tickServer();
        tickDeploy();
    }

    protected List<ServerShip> getAlive(Predicate<ServerShip> predicate){
        return streamAlive().filter(predicate).toList();
    }

    protected List<ServerShip> getAliveWithType(Predicate<SchematicKey> predicate){
        return getAlive(ship -> predicate.test(pool().getTypeOf(ship.getId())));
    }

}

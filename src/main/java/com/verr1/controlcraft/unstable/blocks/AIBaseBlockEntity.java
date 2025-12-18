package com.verr1.controlcraft.unstable.blocks;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.ai.api.IAIListener;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import com.verr1.controlcraft.utils.MathUtils;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import com.verr1.controlcraft.utils.VSMathUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Vec3i;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.joml.Vector3d;
import org.valkyrienskies.core.api.ships.ServerShip;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import static org.valkyrienskies.mod.api.ValkyrienSkies.toJOML;


public class AIBaseBlockEntity extends OnShipBlockEntity implements
        IAIListener
{
    protected double cachedMass = 1;

    public AIBaseBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
    }

    protected void registerDouble(Supplier<Double> getter, Consumer<Double> setter, NetworkKey key){
        register(getter, setter, SerializeUtils.DOUBLE, ClientBuffer.DOUBLE.get(), key);
    }

    protected void registerBoolean(Supplier<Boolean> getter, Consumer<Boolean> setter, NetworkKey key){
        register(getter, setter, SerializeUtils.BOOLEAN, ClientBuffer.BOOLEAN.get(), key);
    }

    protected<T> void register(Supplier<T> getter, Consumer<T> setter, Serializer<T> ser, ClientBuffer<T> buf, NetworkKey key){
        buildRegistry(key)
                .withBasic(SerializePort.of(getter, setter, ser))
                .withClient(buf)
                .register();
    }

    public double currentMass(){
        ServerShip ship = getLoadedServerShip();
        if(ship == null)return 0;
        return ship.getInertiaData().getMass();
    }

    public double originalMass(){
//        long id = getShipOrGroundID();
//        return AIServer.MANAGER
//                .getDataOf(id)
//                .map(d -> d.storageSchematic)
//                .map(AIServer.SCHEMATICS_MANAGER::getLoaded)
//                .map(AISchematic::mass)
//                .orElse(-1.0);
        return cachedMass;
    }

    public Optional<AIBlockNetwork> network(){
        return Optional.ofNullable(getLoadedServerShip()).map(s -> s.getAttachment(AIBlockNetwork.class));
    }

    public void discard(){
        AIServer.MANAGER.discard(getShipOrGroundID());
    }

    public boolean isAI(){
        return AIServer.MANAGER.isAI(getShipOrGroundID());
    }

    public Vector3d getSelfPositionShip(){
        return toJOML(getBlockPos().getCenter());
    }

    public Vector3d getSelfPositionWorld(){
        return readSelf().s2wTransform().transformPosition(getSelfPositionShip());
    }

    @Override
    public void onSpawn() {
        cachedMass = currentMass();
    }

    @Override
    public void lazyTickServer() {
        super.lazyTickServer();
        syncNetwork();
    }

    public Vector3d front(){
        return readSelf().s2wTransform().transformDirection(new Vector3d(0, 0, 1));
    }

    public Vector3d frontDirection(){
        Vec3i v = getDirection().getNormal();
        return readSelf().s2wTransform().transformDirection(v.getX(), v.getY(), v.getZ(), new Vector3d());
    }

    public void syncNetwork(){
        Optional.ofNullable(getLoadedServerShip())
                .map(AIBlockNetwork::get)
                .ifPresent(s -> s.activateListener(getWorldBlockPos(), this));
    }

}

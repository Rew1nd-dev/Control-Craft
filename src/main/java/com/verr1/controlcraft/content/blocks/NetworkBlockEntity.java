package com.verr1.controlcraft.content.blocks;

import com.verr1.controlcraft.foundation.api.Unnamed;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.packets.specific.LazyRequestBlockEntitySyncPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.SyncBlockEntityClientPacket;
import com.verr1.controlcraft.foundation.network.packets.specific.SyncBlockEntityServerPacket;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.network.PacketDistributor;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;

public class NetworkBlockEntity extends SidedTickedBlockEntity {

    /*
    *  simplex channel: sync action raise from server, it's 
    *
    * */

    private final HashMap<NetworkKey, AsymmetricPort> duplex = new HashMap<>();
    private final HashMap<NetworkKey, SymmetricPort> simplex = new HashMap<>();
    private final HashMap<NetworkKey, SymmetricPort> saveLoads = new HashMap<>();


    public NetworkBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);
    }


    @SuppressWarnings("unchecked")
    public <T> @Nullable T readClientBuffer(NetworkKey key, Class<T> clazz){
        if(!duplex.containsKey(key))return null;
        ClientBuffer<?> clientBuffer = duplex.get(key).rx;
        if(clazz.isAssignableFrom(clientBuffer.getClazz())){
            return ((ClientBuffer<T>) clientBuffer).getBuffer();
        }
        return null;
    }

    @SuppressWarnings("unchecked")
    public <T> void writeClientBuffer(NetworkKey key, T value, Class<T> clazz){
        if(!duplex.containsKey(key))return;
        ClientBuffer<?> clientBuffer = duplex.get(key).rx;
        if(clazz.isAssignableFrom(clientBuffer.getClazz())){
            ((ClientBuffer<T>) clientBuffer).setBuffer(value);
        }
    }

    public boolean isAnyDirty(NetworkKey... key){
        AtomicBoolean isAllUpdated = new AtomicBoolean(true);
        Arrays.asList(key).forEach(
                k -> Optional
                        .ofNullable(duplex.get(k))
                        .map(sidePort -> sidePort.rx)
                        .ifPresent(clientBuffer ->
                                isAllUpdated.set(
                                        isAllUpdated.get() & !clientBuffer.isDirty()))
        );
        return !isAllUpdated.get();
    }




    public void syncForPlayer(boolean simplex, ServerPlayer player, NetworkKey... key){
        dispatchChannel(PacketDistributor.PLAYER.with(() -> player), simplex, key);
    }

    public void syncForAllPlayers(boolean simplex, NetworkKey... key){
        dispatchChannel(PacketDistributor.ALL.noArg(), simplex, key);
    }

    public void request(NetworkKey... requests){
        if(level == null || !level.isClientSide)return;
        var p = new LazyRequestBlockEntitySyncPacket(getBlockPos(), List.of(requests));
        ControlCraftPackets.getChannel().sendToServer(p);
    }

    public void receiveRequest(List<NetworkKey> requests, ServerPlayer sender){
        // assume requests are only from duplex channels
        syncForPlayer(false, sender, Arrays.copyOf(requests.toArray(), requests.size(), NetworkKey[].class));
    }

    public void syncForNear(boolean simplex, NetworkKey... key){
        BlockPos pos = getBlockPos();
        dispatchChannel(
                PacketDistributor.NEAR.with(() -> new PacketDistributor.TargetPoint(pos.getX(), pos.getY(), pos.getZ(), 64, level.dimension())),
                simplex,
                key
        );
    }

    public void setDirty(NetworkKey... key){
        Arrays.asList(key).forEach(
                k -> {
                    Optional
                            .ofNullable(duplex.get(k))
                            .ifPresent(rw -> rw.rx.setDirty());

                }
        );
    }

    protected Registry buildRegistry(NetworkKey key){
        return new Registry(key);
    }

    public void syncToServer(NetworkKey... key){
        syncDuplex(PacketDistributor.SERVER.noArg(), key);
    }
    
    
    private void dispatchPacket(PacketDistributor.PacketTarget target, CompoundTag tag){
        if(level == null)return;
        if (!level.isClientSide) {
            var p = new SyncBlockEntityClientPacket(getBlockPos(), tag);
            ControlCraftPackets.getChannel().send(target, p);
        }
        if (level.isClientSide) {
            var p = new SyncBlockEntityServerPacket(getBlockPos(), tag);
            ControlCraftPackets.getChannel().sendToServer(p);
        }
    }

    public void dispatchChannel(PacketDistributor.PacketTarget target, boolean isSimplex, NetworkKey... key){
        if (isSimplex)syncSimplex(target, key);
        else syncDuplex(target, key);
    }
    
    protected void syncSimplex(PacketDistributor.PacketTarget target, NetworkKey... key){
        if(level == null)return;
        CompoundTag syncTag = new CompoundTag();
        Arrays.asList(key).forEach(
                k -> Optional
                        .ofNullable(simplex.get(k))
                        .map(rw -> rw.send(level.isClientSide))
                        .ifPresent(t -> syncTag.put(k.getSerializedName(), t))
        );
        CompoundTag tag = new CompoundTag();
        tag.put("simplex", syncTag);
        dispatchPacket(target, syncTag);
    }
    
    protected void syncDuplex(PacketDistributor.PacketTarget target, NetworkKey... key){
        if(level == null)return;
        CompoundTag portTag = new CompoundTag();
        Arrays.asList(key).forEach(
                k -> Optional
                    .ofNullable(duplex.get(k))
                    .map(rw -> rw.send(level.isClientSide))
                    .ifPresent(t -> portTag.put(k.getSerializedName(), t))
        );
        CompoundTag tag = new CompoundTag();
        tag.put("duplex", portTag);
        dispatchPacket(target, tag);
    }

    public void receiveSync(CompoundTag tag){
        if(level == null)return;
        CompoundTag duplexTag = tag.getCompound("duplex");
        CompoundTag simplexTag = tag.getCompound("simplex");
        if(!duplexTag.isEmpty()){
            duplex.forEach((k, sidePort) -> {
                if(!duplexTag.contains(k.getSerializedName()))return;
                sidePort.dispatch(duplexTag.getCompound(k.getSerializedName()), level.isClientSide);
            });
        }
        if(!simplexTag.isEmpty()){
            simplex.forEach((k, sidePort) -> {
                if(!simplexTag.contains(k.getSerializedName()))return;
                sidePort.dispatch(simplexTag.getCompound(k.getSerializedName()), level.isClientSide);
            });
        }
        if(level.isClientSide)return;
        setChanged();
    }

    @Override
    protected void read(CompoundTag compound, boolean clientPacket) {
        super.read(compound, clientPacket);
        if(clientPacket)return;
        CompoundTag saveloads = compound.getCompound("saveloads");
        saveLoads.forEach((k, sidePort) -> {
            if(compound.contains(k.getSerializedName())){
                sidePort.dispatch(saveloads.getCompound(k.getSerializedName()), false);
            }
        });
    }

    @Override
    protected void write(CompoundTag compound, boolean clientPacket) {
        super.write(compound, clientPacket);
        if(clientPacket)return;
        CompoundTag saveloads = new CompoundTag();
        saveLoads.forEach((k, sidePort) -> {
            saveloads.put(k.getSerializedName(), sidePort.send(false));
        });
        compound.put("saveloads", saveloads);
    }


    public static class AsymmetricPort implements SidePort{
        ClientBuffer<?> rx;
        Unnamed<CompoundTag> tx;

        public AsymmetricPort(ClientBuffer<?> rx, Unnamed<CompoundTag> tx){
            this.rx = rx;
            this.tx = tx;
        }

        @Override
        public Unnamed<CompoundTag> client() {
            return rx;
        }

        @Override
        public Unnamed<CompoundTag> server() {
            return tx;
        }
    }

    public static class SymmetricPort implements SidePort{
        Unnamed<CompoundTag> trx;
        public SymmetricPort(Unnamed<CompoundTag> trx){
            this.trx = trx;
        }
        @Override
        public Unnamed<CompoundTag> client() {
            return trx;
        }
        @Override
        public Unnamed<CompoundTag> server() {
            return trx;
        }
    }

    public interface SidePort {

        Unnamed<CompoundTag> client();

        Unnamed<CompoundTag> server();

        default void dispatch(CompoundTag tag, boolean isClientside){
            if(isClientside){
                client().set(tag);
            }else{
                server().set(tag);
            }
        }

        default CompoundTag send(boolean isClientside){
            if(isClientside){
                return client().get();
            }else{
                return server().get();
            }
        }



    }


    private void registerAsymmetric(
            NetworkKey key,
            Unnamed<CompoundTag> server,
            ClientBuffer<?> client
    ){
        duplex.put(key, new AsymmetricPort(client, server));
    }

    private void registerSaveLoads(
            NetworkKey key,
            Unnamed<CompoundTag> server
    ){
        saveLoads.put(key, new SymmetricPort(server));
    }

    private void registerSync(
            NetworkKey key,
            Unnamed<CompoundTag> server
    ){
        simplex.put(key, new SymmetricPort(server));
    }

    protected class Registry {
        Unnamed<CompoundTag> server = Unnamed.createEmpty(CompoundTag.class);
        ClientBuffer<?> client = null;
        NetworkKey key;
        boolean asSaveLoad = true;
        boolean dispatchToBuffer = false;
        boolean dispatchToSync = false;

        public Registry(NetworkKey key){
            this.key = key;
        }

        public Registry withBasic(Unnamed<CompoundTag> server){
            this.server = server;
            return this;
        }

        public Registry withClient(ClientBuffer<?> client){
            dispatchToBuffer = true;
            this.client = client;
            return this;
        }

        public Registry dispatchToSync(){
            dispatchToSync = true;
            return this;
        }

        public Registry runtimeOnly(){
            asSaveLoad = false;
            return this;
        }

        // For save load: Unnamed<CompoundTag> only calls when read write()
        // For sync: Just like what I did before, server write and client read, they use the same serializer
        // For buffer: server write and client buffer read the tag

        public void register(){
            if(asSaveLoad)registerSaveLoads(key, server);
            if(dispatchToSync)registerSync(key, server);
            if(dispatchToBuffer)registerAsymmetric(key, server, client);
        }


    }

}

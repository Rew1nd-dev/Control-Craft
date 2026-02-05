package com.verr1.controlcraft.content.links.connector;

import com.verr1.controlcraft.content.blocks.OnShipBlockEntity;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkBus;
import com.verr1.controlcraft.content.valkyrienskies.attachments.CimulinkPorts;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.foundation.network.remote.RemotePort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.ConstraintClusterUtil;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class EasyConnectorBlockEntity extends OnShipBlockEntity {

    public static final NetworkKey CONNECT = NetworkKey.create("connect");
    public static final NetworkKey DISCONNECT = NetworkKey.create("disconnect");
    public static final NetworkKey INFO = NetworkKey.create("status");

    public EasyConnectorBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(INFO)
                .withBasic(SerializePort.of(this::getLinkStatus, $ -> {}, Status.STATUS))
                .withClient(new ClientBuffer<>(
                                Status.STATUS,
                                Status.class
                )).register();
        panel().register(CONNECT, RemotePort.of(ConnectionStatus.class, this::connect, ConnectionStatus.CS));
        panel().register(DISCONNECT, RemotePort.of(ConnectionStatus.class, this::disconnect, ConnectionStatus.CS));
    }

    public Stream<CimulinkPorts> linkStorages(){
        return  ConstraintClusterUtil.clusterOf(getShipOrGroundID())
            .stream()
            .map(ConstraintClusterUtil::getShipOf)
            .filter(Optional::isPresent)
            .flatMap(Optional::stream)
            .map(CimulinkPorts::getOrCreate);
    }


    public Set<WorldBlockPos> getLinkPositions(){
        return linkStorages().flatMap(b -> b.getAll().stream()).collect(Collectors.toSet());
    }


    public Status getLinkStatus(){
        Set<WorldBlockPos> wbp = getLinkPositions();
        return new Status(
                wbp.stream().map(LinkStatus::of).filter(Objects::nonNull).toList(),
                ConnectionStatus.of(wbp)
        );
    }


    public void connect(ConnectionStatus cs){
        connect(cs.outputPos(), cs.outputName(), cs.inputPos(), cs.inputName());
    }

    public void disconnect(ConnectionStatus cs){
        disconnect(cs.outputPos(), cs.outputName(), cs.inputPos(), cs.inputName());
    }

    public void connect(WorldBlockPos out, String outName, WorldBlockPos in, String inName){
        try{
            BlockLinkPort.of(out).ifPresent(o -> o.connectTo(outName, in, inName));
        }catch (IllegalArgumentException ignored){

        }
    }

    public void disconnect(WorldBlockPos out, String outName, WorldBlockPos in, String inName){
        try{
            BlockLinkPort.of(out).ifPresent(o -> o.disconnectOutput(outName, new BlockPort(in, inName)));
        }catch (IllegalArgumentException ignored){

        }
    }




    public record LinkStatus(
            List<String> inputNames,
            List<String> outputNames,
            WorldBlockPos pos,
            String name
    ){
        public static final Serializer<List<String>> STRING_LIST = SerializeUtils.ofList(SerializeUtils.STRING);
        public static final Serializer<WorldBlockPos> POS = SerializeUtils.of(WorldBlockPos::serialize, WorldBlockPos::deserialize);
        public static final Serializer<LinkStatus> LS = SerializeUtils.of(LinkStatus::serialize, LinkStatus::deserialize);

        public static @Nullable LinkStatus of(WorldBlockPos pos){
            BlockLinkPort blp = BlockLinkPort.of(pos).orElse(null);
            if(blp == null)return null;
            return new  LinkStatus(
                    List.copyOf(blp.inputsNamesExcludeSignals()),
                    List.copyOf(blp.outputsNames()),
                    pos,
                    blp.name()
            );
        }

        public CompoundTag serialize(){
            return new CompoundTagBuilder()
                    .withCompound("inputNames", STRING_LIST.serialize(inputNames))
                    .withCompound("outputNames", STRING_LIST.serialize(outputNames))
                    .withCompound("pos", POS.serialize(pos))
                    .withCompound("name", SerializeUtils.STRING.serialize(name))
                    .build();
        }

        public static LinkStatus deserialize(CompoundTag tag){
            return new LinkStatus(
                    STRING_LIST.deserialize(tag.getCompound("inputNames")),
                    STRING_LIST.deserialize(tag.getCompound("outputNames")),
                    POS.deserialize(tag.getCompound("pos")),
                    SerializeUtils.STRING.deserialize(tag.getCompound("name"))
            );
        }

    }

    public record ConnectionStatus(
            WorldBlockPos outputPos,
            WorldBlockPos inputPos,
            String outputName,
            String inputName
    ) {
        public static final Serializer<WorldBlockPos> POS = SerializeUtils.of(WorldBlockPos::serialize, WorldBlockPos::deserialize);
        public static final Serializer<ConnectionStatus> CS = SerializeUtils.of(ConnectionStatus::serialize, ConnectionStatus::deserialize);


        public static List<ConnectionStatus> of(Set<WorldBlockPos> blps){
            return blps.stream()
                    .map(b -> BlockLinkPort.of(b).orElse(null))
                    .filter(Objects::nonNull)
                    .flatMap(
                            blp -> blp.backwardLinks()
                                    .entrySet()
                                    .stream()
                                    .filter(e -> blps.contains(e.getValue().pos()))
                                    .map(e -> new ConnectionStatus(
                                            e.getValue().pos(),
                                            blp.pos(),
                                            e.getValue().portName(),
                                            e.getKey()
                                    ))
                    )
                    .toList();
        }

        public CompoundTag serialize() {
            return new CompoundTagBuilder()
                    .withCompound("outputPos", POS.serialize(outputPos))
                    .withCompound("inputPos", POS.serialize(inputPos))
                    .withCompound("outputName", SerializeUtils.STRING.serialize(outputName))
                    .withCompound("inputName", SerializeUtils.STRING.serialize(inputName))
                    .build();
        }

        public static ConnectionStatus deserialize(CompoundTag tag) {
            return new ConnectionStatus(
                    POS.deserialize(tag.getCompound("outputPos")),
                    POS.deserialize(tag.getCompound("inputPos")),
                    SerializeUtils.STRING.deserialize(tag.getCompound("outputName")),
                    SerializeUtils.STRING.deserialize(tag.getCompound("inputName"))
            );
        }
    }

    public record Status(
            List<LinkStatus> allCimulinks,
            List<ConnectionStatus> allConnections
    ) {
        public static final Status EMPTY = new Status(List.of(), List.of());
        public static final Serializer<List<LinkStatus>> LINK_STATUS_LIST = SerializeUtils.ofList(LinkStatus.LS);
        public static final Serializer<List<ConnectionStatus>> CONNECTION_STATUS_LIST = SerializeUtils.ofList(ConnectionStatus.CS);
        public static final Serializer<Status> STATUS = SerializeUtils.of(Status::serialize, Status::deserialize);

        public CompoundTag serialize() {
            return new CompoundTagBuilder()
                    .withCompound("allCimulinks", LINK_STATUS_LIST.serialize(allCimulinks))
                    .withCompound("allConnections", CONNECTION_STATUS_LIST.serialize(allConnections))
                    .build();
        }

        public static Status deserialize(CompoundTag tag) {
            return new Status(
                    LINK_STATUS_LIST.deserialize(tag.getCompound("allCimulinks")),
                    CONNECTION_STATUS_LIST.deserialize(tag.getCompound("allConnections"))
            );
        }
    }
}

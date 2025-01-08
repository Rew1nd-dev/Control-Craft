package com.verr1.vscontrolcraft.registry;

import com.simibubi.create.foundation.networking.SimplePacketBase;
import com.verr1.vscontrolcraft.ControlCraft;
import com.verr1.vscontrolcraft.base.SyncAnimationPacket;
import com.verr1.vscontrolcraft.blocks.propeller.PropellerBlockEntity;
import com.verr1.vscontrolcraft.blocks.recevier.ReceiverOpenScreenPacket;
import com.verr1.vscontrolcraft.blocks.recevier.ReceiverRegisterPacket;
import com.verr1.vscontrolcraft.blocks.spinalyzer.SpinalyzerLinkPacket;
import com.verr1.vscontrolcraft.blocks.wingController.WingControllerBlockEntity;
import net.minecraft.core.BlockPos;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraftforge.network.NetworkDirection;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.PacketDistributor;
import net.minecraftforge.network.simple.SimpleChannel;

import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public enum AllPackets {
    // Client To Server
    NETWORK_ID_SETTING(ReceiverRegisterPacket.class, ReceiverRegisterPacket::new, NetworkDirection.PLAY_TO_SERVER),
    SPINALYZER_TARGET_SELECTION(SpinalyzerLinkPacket.class, SpinalyzerLinkPacket::new, NetworkDirection.PLAY_TO_SERVER),

    //Server To Client
    RECEIVER_SCREEN_OPEN(ReceiverOpenScreenPacket.class, ReceiverOpenScreenPacket::new, NetworkDirection.PLAY_TO_CLIENT),
    SYNC_PROPELLER_ANIMATION(SyncAnimationPacket.getPacketClass(),  SyncAnimationPacket.createDecoder(PropellerBlockEntity.class, new PropellerBlockEntity.PropellerAnimationDataHandler()), NetworkDirection.PLAY_TO_CLIENT),
    SYNC_WING_CONTROLLER_ANIMATION(SyncAnimationPacket.getPacketClass(),  SyncAnimationPacket.createDecoder(WingControllerBlockEntity.class, new WingControllerBlockEntity.WingControllerAnimationDataHandler()), NetworkDirection.PLAY_TO_CLIENT);


    public static final String NETWORK_VERSION = "1.2";

    private static SimpleChannel channel;

    private PacketType<?> packetType;

    <T extends SimplePacketBase> AllPackets(Class<T> type, Function<FriendlyByteBuf, T> factory,
                                            NetworkDirection direction) {
        packetType = new PacketType<>(type, factory, direction);
    }

    public static void registerPackets() {
        channel = NetworkRegistry.ChannelBuilder
                .named(new ResourceLocation(ControlCraft.MODID, ControlCraft.MODID+"_channel")).networkProtocolVersion(() -> {
                    return NETWORK_VERSION;
                })
                .clientAcceptedVersions(NETWORK_VERSION::equals).serverAcceptedVersions(NETWORK_VERSION::equals).simpleChannel();

        for (AllPackets packet : values())
            packet.packetType.register();
    }

    public static SimpleChannel getChannel() {
        return channel;
    }

    public static void sendToNear(Level world, BlockPos pos, int range, Object message) {
        getChannel().send(
                PacketDistributor.NEAR.with(PacketDistributor.TargetPoint.p(pos.getX(), pos.getY(), pos.getZ(), range, world.dimension())),
                message);
    }

    public static <MSG> void sendToPlayer(MSG message, ServerPlayer player) {
        channel.send(PacketDistributor.PLAYER.with(() -> player), message);
    }


    private static class PacketType<T extends SimplePacketBase> {
        private static int index = 0;

        private BiConsumer<T, FriendlyByteBuf> encoder;
        private Function<FriendlyByteBuf, T> decoder;
        private BiConsumer<T, Supplier<NetworkEvent.Context>> handler;
        private Class<T> type;
        private NetworkDirection direction;

        private PacketType(Class<T> type, Function<FriendlyByteBuf, T> factory, NetworkDirection direction) {
            encoder = T::write;
            decoder = factory;
            handler = (packet, contextSupplier) -> {
                NetworkEvent.Context context = contextSupplier.get();
                if (packet.handle(context)) {
                    context.setPacketHandled(true);
                }
            };
            this.type = type;
            this.direction = direction;
        }

        private void register() {
            getChannel().messageBuilder(type, index++, direction)
                    .encoder(encoder)
                    .decoder(decoder)
                    .consumerNetworkThread(handler)
                    .add();
        }
    }

}

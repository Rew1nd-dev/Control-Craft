package com.verr1.controlcraft.foundation.managers;

import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.executor.executables.ConditionExecutable;
import com.verr1.controlcraft.foundation.network.packets.BlockBoundServerPacket;
import com.verr1.controlcraft.foundation.type.RegisteredPacketType;
import com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies;
import com.verr1.controlcraft.registry.ControlCraftPackets;
import net.minecraft.client.CameraType;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.Nullable;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

@OnlyIn(Dist.CLIENT)
public class ClientCameraManager {
    private static BlockPos LinkCameraPos;



    private static Vec3 QueryPos;

    private static Vec3 latestCameraWorldPos = null;

    public static @Nullable CameraBlockEntity getLinkedCamera(){
        return Optional
                .ofNullable(LinkCameraPos)
                .flatMap(pos -> Optional
                                .ofNullable(Minecraft.getInstance().level)
                                .flatMap(level -> Optional
                                                    .ofNullable(level.getBlockEntity(pos))))
                .filter(CameraBlockEntity.class::isInstance)
                .map(CameraBlockEntity.class::cast)
                .orElse(null);
    }

    public static boolean isLinked() {
        return LinkCameraPos != null;
    }

    public static @Nullable BlockPos getLinkCameraPos(){
        return LinkCameraPos;
    }

    public static @Nullable Vec3 getLinkOrQueryCameraWorldPosition(){
        return latestCameraWorldPos == null ? QueryPos() : latestCameraWorldPos;
    }

    public static Vec3 QueryPos() {
        Level level = Minecraft.getInstance().level;
        if(level == null || QueryPos == null)return null;

        return Optional.of(QueryPos)
                .map(p -> ValkyrienSkies.getShipManagingBlock(Minecraft.getInstance().level, BlockPos.containing(p)))
                .map(s -> s.getTransform().getShipToWorld())
                .map(t -> t.transformPosition(toJOML(QueryPos)))
                .map(ValkyrienSkies::toMinecraft)
                .orElse(QueryPos);

    }

    public static void linkDirect(BlockPos cameraPos){
        LinkCameraPos = cameraPos;
        Minecraft.getInstance().options.bobView().set(false);
    }

    public static void linkWithAck(BlockPos cameraPos){
        LocalPlayer player = Minecraft.getInstance().player;
        if(player == null)return;
        var p = new BlockBoundServerPacket.builder(cameraPos, RegisteredPacketType.SETTING_0)
                .withDouble(0)
                .withDouble(0)
                .withUtf8(player.getName().getString())
                .build();
        ControlCraftPackets.getChannel().sendToServer(p);


        QueryPos = cameraPos.getCenter();
        var task = new ConditionExecutable
                .builder(() -> linkDirect(cameraPos))
                .withCondition(() -> BlockEntityGetter.getLevelBlockEntityAt(player.clientLevel, cameraPos, CameraBlockEntity.class).isPresent())
                .withExpirationTicks(40)
                .withOrElse(
                        () -> {
                            QueryPos = null;
                            player.sendSystemMessage(Component.literal("Camera Failed To Load"));
                        }
                )
                .build();

        ControlCraftClient.CLIENT_EXECUTOR.execute(task);
    }

    public static void deLink(){
        disconnectServerCamera();
        LinkCameraPos = null;
        QueryPos = null;
        Minecraft.getInstance().options.bobView().set(true);
        Minecraft.getInstance().options.setCameraType(CameraType.FIRST_PERSON);
        Minecraft.getInstance().levelRenderer.allChanged();
        setLatest(null);
    }

    private static void setLatest(Vec3 latest){
        latestCameraWorldPos = latest;
    }

    public static Vec3 latestCameraWorldPos() {
        return latestCameraWorldPos;
    }

    public static void disconnectServerCamera(){
        if(LinkCameraPos == null)return;
        var p = new BlockBoundServerPacket.builder(LinkCameraPos, RegisteredPacketType.EXTEND_0)
                .build();
        try{
            ControlCraftPackets.getChannel().sendToServer(p);
        }catch (NullPointerException ignored){
            // when players close game when they are in camera,
            // it throws this exception because getConnection() is null
        }
    }


    public static void tick(){
        CameraBlockEntity camera = getLinkedCamera();
        if(camera == null && isLinked()){
            deLink();
        }
        LocalPlayer player = Minecraft.getInstance().player;
        if(player == null)return;

        if(camera != null && isLinked()){
            camera.setPitch(player.getViewXRot(1));
            camera.setYaw(player.getViewYRot(1));
            camera.syncServer(player.getName().getString());
            setLatest(toMinecraft(camera.getCameraPosition()));
        }


        if(isLinked()){
            Minecraft.getInstance().options.setCameraType(CameraType.THIRD_PERSON_BACK);
        }
        if(player.input.jumping && isLinked()){
            deLink();
        }

    }

}

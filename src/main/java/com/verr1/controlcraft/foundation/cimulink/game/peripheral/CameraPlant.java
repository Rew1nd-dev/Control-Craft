package com.verr1.controlcraft.foundation.cimulink.game.peripheral;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.blocks.camera.CameraBlockEntity;
import net.minecraft.core.BlockPos;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class CameraPlant extends MutablePlant{

    private final BlockPos posToken;
    private final CameraBlockEntity cbe;
    private final Vector3d cachedPosition = new Vector3d();
    private final Vector3d cachedVelocity = new Vector3d();
    private boolean toggle = false;

    public CameraPlant(CameraBlockEntity cbe) {
        super(new builder()
                .in("clipNewBlock",
                    (self, v) -> schedule(
                        cast(self).token("Block"),
                        () -> {if(cast(self).toggle(v))cast(self).clipNewBlock();}
                    )
                )
                .in("clipNewEntity",
                    (self, v) -> schedule(
                        cast(self).token("Entity"),
                        () -> {if(cast(self).toggle(v))cast(self).clipNewEntity();}
                    )
                )
                .in("clipNewShip",
                    (self, v) -> schedule(
                        cast(self).token("Ship"),
                        () -> {if(cast(self).toggle(v))cast(self).clipNewShip();}
                    )
                )
                .in("clipNewPlayer",
                    (self, v) -> schedule(
                        cast(self).token("Player"),
                        () -> {if(cast(self).toggle(v))cast(self).clipNewServerPlayer();}
                    )
                )
                .out("latest_x",  self -> cast(self).cachedPosition().x())
                .out("latest_y",  self -> cast(self).cachedPosition().y())
                .out("latest_z",  self -> cast(self).cachedPosition().z())
                .out("latest_vx", self -> cast(self).cachedVelocity().x())
                .out("latest_vy", self -> cast(self).cachedVelocity().y())
                .out("latest_vz", self -> cast(self).cachedVelocity().z())

                .out("yaw", self -> cbe.getYaw())
                .out("pitch", self -> cbe.getPitch())
                .out("abs_yaw", self -> cbe.getTransformedYaw())
                .out("abs_pitch", self -> cbe.getTransformedPitch())
                .out("abs_view_x", self -> cbe.getAbsViewForward().x())
                .out("abs_view_y", self -> cbe.getAbsViewForward().y())
                .out("abs_view_z", self -> cbe.getAbsViewForward().z())

        );
        posToken = cbe.getBlockPos();
        this.cbe = cbe;
    }

    public boolean toggle(double v){
        toggle = v > 0.5 && !toggle;
        return toggle;
    }

    public String token(String extra){
        return posToken.toShortString() + "-" + extra;
    }

    public void clipNewBlock(){
        cbe.clipNewBlock();

    }

    public void clipNewEntity(){
        cbe.clipNewEntity();
    }

    public void clipNewServerPlayer(){
        cbe.clipNewServerPlayer();
    }

    public Vector3dc cachedVelocity(){
        return cachedVelocity;
    }

    public Vector3dc cachedPosition(){
        return cachedPosition;
    }

    public void clipNewShip(){
        cbe.clipNewShip();
    }

    @Override
    protected void prePositiveEdge() {
        cachedPosition.set(cbe.latestClipPosition());
        cachedVelocity.set(cbe.latestClipVelocity());
    }

    public static CameraPlant cast(MutablePlant _super){
        return (CameraPlant) _super;
    }

    public static void schedule(String token, Runnable task){
        if(ControlCraftServer.onMainThread()){
            task.run();
        }else {
            ControlCraftServer.SERVER_EXECUTOR.executeLaterIfAbsent(token, task, 1);
        }
    }

}

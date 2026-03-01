package com.verr1.controlcraft.utils;

import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import net.minecraft.core.BlockPos;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;
import org.valkyrienskies.core.apigame.world.ServerShipWorldCore;
import org.valkyrienskies.mod.common.VSGameUtilsKt;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class VSAccessUtils {

    @NotNull
    public static Vector3dc velocity(Vector3dc blockPositionShip, ServerLevel level){
        BlockPos blockPos = BlockPos.containing(toMinecraft(blockPositionShip));
        ServerShip ship = getShipAt(WorldBlockPos.of(level, blockPos)).orElse(null);;
        if(ship == null)return new Vector3d();
        Vector3dc sv_wc = ship.getVelocity();
        Vector3dc sw_wc = ship.getOmega();
        Vector3dc r_sc = new Vector3d(blockPositionShip).sub(ship.getTransform().getPositionInShip());
        Vector3dc r_wc = ship.getShipToWorld().transformDirection(r_sc, new Vector3d());
        return new Vector3d(sv_wc).add(new Vector3d(sw_wc).cross(r_wc));
    }

    public static Optional<ServerShip> getShipAt(WorldBlockPos pos){
        return Optional.ofNullable(VSGameUtilsKt.getShipManagingPos(pos.level(server()), pos.pos())
        );
    }

    private static ServerShipWorldCore vsWorld(){
        return Objects.requireNonNull(VSGameUtilsKt.getShipObjectWorld(server()));
    }

    public static Optional<ServerShip> getShipOf(long id){
        return Optional.ofNullable(vsWorld().getAllShips().getById(id));
    }

    public static MinecraftServer server(){
        return ControlCraftServer.INSTANCE;
    }

    public static @NotNull List<ServerShip> getAllShips(){
        return vsWorld().getAllShips().stream().toList();
    }



    public static Vector3dc aimPredict(Vector3dc p_t, Vector3dc v_t, Vector3dc p_c, double v_b){

        Vector3d p_rel = new Vector3d(p_t).sub(p_c);

        // 计算目标速度的平方模长
        double v_t_mag_sq = v_t.lengthSquared();

        // 计算二次方程的系数
        double a = v_t_mag_sq - (v_b * v_b);
        double b = 2.0 * v_t.dot(p_rel);
        double c = p_rel.lengthSquared();

        // 计算判别式
        double delta = b * b - 4 * a * c;

        // 如果判别式小于0，无实数解
        if (delta < 0) {
            return null;
        }

        // 计算两个可能的时间解
        double sqrtDelta = Math.sqrt(delta);
        double t1 = (-b + sqrtDelta) / (2 * a);
        double t2 = (-b - sqrtDelta) / (2 * a);

        // 寻找最小的正时间解
        double t_hit = Double.POSITIVE_INFINITY;
        if (t1 > 0) t_hit = t1;
        if (t2 > 0 && t2 < t_hit) t_hit = t2;

        // 如果没有有效解
        if (t_hit == Double.POSITIVE_INFINITY) {
            return null;
        }

        // 计算目标在命中时刻的位置: p_t + v_t * t_hit
        return new Vector3d(v_t).mul(t_hit).add(p_t);
    }
}

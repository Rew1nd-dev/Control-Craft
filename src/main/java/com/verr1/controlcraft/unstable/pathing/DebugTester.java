package com.verr1.controlcraft.unstable.pathing;

import com.verr1.controlcraft.ControlCraftClient;
import com.verr1.controlcraft.foundation.managers.ClientOutliner;
import com.verr1.controlcraft.unstable.pathing.dubins.DubinsCalculatorV2;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.utils.MathUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.world.entity.player.Player;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;

import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toJOML;
import static com.verr1.controlcraft.foundation.vsapi.ValkyrienSkies.toMinecraft;

public class DebugTester {

    public static final Color[] colors = {
            new Color(0xFF0000), // Red
            new Color(0x00FF00), // Green
            new Color(0x0000FF), // Blue
            new Color(0xFFFF00), // Yellow
            new Color(0xFF00FF), // Magenta
            new Color(0x00FFFF), // Cyan
            new Color(0xFFFFFF)  // White
    };

    public static IPath currentPath = null;
    public static Vector3d currentStart = new Vector3d();
    public static Vector3d currentStartHeading = new Vector3d();
    public static Vector3d currentEnd = new Vector3d();
    public static Vector3d currentEndHeading = new Vector3d();

    public static double radius = 10;

    public static void clientTick(){
        Player p = Minecraft.getInstance().player;
        if(currentPath == null || p == null)return;

        double delta = currentPath.length() / 60;
        for (int i = 0; i < 60; i++) {
            Vector3dc point = currentPath.point(delta * i);
            Vector3dc pointForward = currentPath.point(delta * (i - 1));
            ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine("debug_line_" + i, toMinecraft(point), toMinecraft(pointForward), 10).colored(colors[i % colors.length].getRGB());
        }
        Vector3dc close = currentPath.closestTo(toJOML(p.position()));
        ClientOutliner.drawOutline(
                toMinecraft(MathUtils.centerWithRadius(close, 1)),
                Color.RED.getRGB(),
                "debug_close_target",
                4.0,
                1f / 16
        );
    }

    public static void compute(){

        currentPath = DubinsCalculatorV2.dubins(
                currentStart,
                currentStartHeading,
                currentEnd,
                currentEndHeading,
                radius
        );
    }

    public static void setStart(Vector3dc start, Vector3dc startHeading) {
        currentStart.set(start);
        currentStartHeading.set(startHeading);
        compute();
    }

    public static void setEnd(Vector3dc end, Vector3dc endHeading) {
        currentEnd.set(end);
        currentEndHeading.set(endHeading);
        compute();
    }

    public static void setRadius(double r) {
        radius = r;
        compute();
    }


}

package com.verr1.controlcraft.unstable.pathing;

import com.verr1.controlcraft.ControlCraftClient;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.awt.*;

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

    public static LerpPath<Vector3dc> currentPath = null;
    public static Vector3d currentStart = new Vector3d();
    public static Vector3d currentStartHeading = new Vector3d();
    public static Vector3d currentEnd = new Vector3d();
    public static Vector3d currentEndHeading = new Vector3d();

    public static double radius = 10;

    public static void clientTick(){
        if(currentPath == null)return;

        for (int i = 1; i < currentPath.segments(); i++){
            Vector3dc point = currentPath.lerp(i);
            Vector3dc pointForward = currentPath.lerp(i - 1);
            ControlCraftClient.CLIENT_LERPED_OUTLINER.showLine("debug_line_" + i, toMinecraft(point), toMinecraft(pointForward), 10).colored(colors[i % colors.length].getRGB());
        }
    }

    public static void compute(){
        /*
        * currentPath = Navigator.dubins(
                currentStart, currentStartHeading,
                currentEnd, currentEndHeading,
                radius
        );*/
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

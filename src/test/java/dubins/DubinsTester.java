package dubins;


import com.verr1.controlcraft.unstable.pathing.dubins.DubinsCalculator;
import org.joml.Vector2d;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class DubinsTester {

    public static void main(String[] args) {
        // 测试案例1：基本LSL路径
        testDubinsPath(
                new Vector2d(0, 0), 0,
                new Vector2d(5, 5), Math.PI/2,
                2.0,
                "case1.txt"
        );

        // 测试案例2：RSR路径
        testDubinsPath(
                new Vector2d(0, 0), 0,
                new Vector2d(5, 0), Math.PI,
                2.0,
                "case2.txt"
        );

        // 测试案例3：LSR路径
        testDubinsPath(
                new Vector2d(0, 0), 0,
                new Vector2d(5, 5), Math.PI,
                2.0,
                "case3.txt"
        );

        // 测试案例4：RSL路径
        testDubinsPath(
                new Vector2d(0, 0), Math.PI/2,
                new Vector2d(5, 5), 0,
                2.0,
                "case4.txt"
        );

        // 测试案例5：短距离路径
        testDubinsPath(
                new Vector2d(0, 0), 0,
                new Vector2d(1, 1), Math.PI/2,
                2.0,
                "case5.txt"
        );

        // 测试案例6：大角度转向
        testDubinsPath(
                new Vector2d(0, 0), 0,
                new Vector2d(0, 5), Math.PI,
                2.0,
                "case6.txt"
        );
    }

    private static void testDubinsPath(
            Vector2d startPos, double startHeading,
            Vector2d goalPos, double goalHeading,
            double turningRadius,
            String filename
    ) {
        DubinsCalculator.DubinsPathResult path = DubinsCalculator.calculateDubinsPath(
                startPos, startHeading,
                goalPos, goalHeading,
                turningRadius
        );

        if (path == null) {
            System.out.println("No path found for " + filename);
            return;
        }

        try (PrintWriter writer = new PrintWriter(new FileWriter(filename))) {
            int segments = path.segments();

            // 写入起点和终点
            writer.println("# Start: " + startPos);
            writer.println("# Goal: " + goalPos);
            writer.println("# Type: " + path.pathType);
            writer.println("# Total length: " + path.totalLength);
            writer.println("# Points:");

            // 采样路径点
            for (int i = 0; i <= segments; i++) {
                Vector2d point = path.lerp(i);
                writer.println(point.x + "," + point.y);
            }

            System.out.println("Generated " + filename + " with " + (segments + 1) + " points");
        } catch (IOException e) {
            System.err.println("Error writing to " + filename + ": " + e.getMessage());
        }
    }
}

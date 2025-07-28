package dubins;

import com.verr1.controlcraft.unstable.pathing.dubins.DubinsCalculator;
import org.joml.Vector2d;
import org.joml.Vector2dc;

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.DecimalFormat;

public class DubinsDetailedTester {

    private static final DecimalFormat df = new DecimalFormat("0.000");

    public static void main(String[] args) {
        // 测试案例
        testDubinsPath(
                new Vector2d(0, 0), 0,
                new Vector2d(5, 5), Math.PI/2,
                1.0,
                "detailed_case1.txt"
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
            // 写入元数据
            writer.println("# Start: " + formatVector(startPos) + ", Heading: " + df.format(Math.toDegrees(startHeading)) + "°");
            writer.println("# Goal: " + formatVector(goalPos) + ", Heading: " + df.format(Math.toDegrees(goalHeading)) + "°");
            writer.println("# Path Type: " + path.pathType);
            writer.println("# Total Length: " + df.format(path.totalLength));
            writer.println("# Turning Radius: " + turningRadius);

            // 写入路径段信息
            writer.println("# Segments:");
            for (int i = 0; i < path.segments.size(); i++) {
                DubinsCalculator.PathSegment segment = path.segments.get(i);
                writer.println("Segment " + (i + 1) + ": " + segment.type);
                writer.println("  Length: " + df.format(segment.length));
                writer.println("  Start: " + formatVector(segment.startPoint));
                writer.println("  End: " + formatVector(segment.endPoint));

                if (segment.type != DubinsCalculator.SegmentType.STRAIGHT) {
                    writer.println("  Center: " + formatVector(segment.center));
                    writer.println("  Radius: " + df.format(segment.radius));
                    writer.println("  StartAngle: " + df.format(Math.toDegrees(segment.startAngle)) + "°");
                    writer.println("  EndAngle: " + df.format(Math.toDegrees(segment.endAngle)) + "°");
                }
            }

            System.out.println("Generated detailed path: " + filename);
        } catch (IOException e) {
            System.err.println("Error writing to " + filename + ": " + e.getMessage());
        }
    }

    private static String formatVector(Vector2d vec) {
        return "(" + df.format(vec.x) + ", " + df.format(vec.y) + ")";
    }

    private static String formatVector(Vector2dc vec) {
        return "(" + df.format(vec.x()) + ", " + df.format(vec.y()) + ")";
    }
}
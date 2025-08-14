package path;

import com.verr1.controlcraft.unstable.pathing.path.ArcPath;
import com.verr1.controlcraft.unstable.pathing.path.CombinedPath;
import com.verr1.controlcraft.unstable.pathing.path.IPath;
import com.verr1.controlcraft.unstable.pathing.path.LinePath;
import org.joml.Matrix4d;
import org.joml.Vector2d;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import java.util.List;

public class PathTest {

    public static void test0(){
        IPath p0 = new LinePath(new Vector3d(0, 0, 0), new Vector3d(4, 5, 6));
        IPath p = new ArcPath(1, -1, 5, new Matrix4d(), true);

        Vector3dc ob0 = new Vector3d(5, 1, 1);
        Vector3dc ob1 = new Vector3d(-12, -12, 12);

        Vector3dc cl0 = p.closestTo(ob0);
        Vector3dc cl1 = p.closestTo(ob1);

        double dl0 = p.closestDistanceFromStart(ob0);
        double dl1 = p.closestDistanceFromStart(ob1);

        Vector3dc _cl0 = p.point(dl0);
        Vector3dc _cl1 = p.point(dl1);

        System.out.println(p.start());
        System.out.println(p.end());
        System.out.println(p.length());
        System.out.println("dl0 -- dl1");
        System.out.println(dl0);
        System.out.println(dl1);
        System.out.println("cl0 -- _cl0");
        System.out.println(cl0);
        System.out.println(_cl0);
        System.out.println("cl1 -- _cl1");
        System.out.println(cl1);
        System.out.println(_cl1);
    }

    public static void test1(){
        ArcPath p = new ArcPath(1, -1, 5, new Matrix4d().translate(new Vector3d(1, 2, -4)).rotateXYZ(new Vector3d(0.5, 1, -2)), true);

        var v = p.toWorld(new Vector2d(0, 0));
        System.out.println(v);
    }

    public static void main(String[] args) {
        LinePath p0 = new LinePath(new Vector3d(0, 0, 0), new Vector3d(4, 5, 6));
        LinePath p1 = new LinePath(new Vector3d(4, 5, 6), new Vector3d(14, 15, 16));
        CombinedPath cb = new CombinedPath(List.of(p0, p1));
        System.out.println(cb.indexOf(-1));
        System.out.println(cb.indexOf(1));
        System.out.println(cb.indexOf(15));
        System.out.println(cb.indexOf(65));
    }

}

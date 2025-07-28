package com.verr1.controlcraft.unstable.valkyrienskies.context;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.unstable.pathing.LerpPathV2;
import com.verr1.controlcraft.utils.MathUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Vector3d;
import org.joml.Vector3dc;

public class CruiserControllerV2 {

    private volatile @Nullable LerpPathV2<Vector3dc> currentPath = null;
    private volatile @Nullable LerpPathV2<Vector3dc> nextPath = null;

    private double cutoff = 0;
    private double speed = 20;
    private volatile double travelled = 0;

    private final Vector3d latestNonNull = new Vector3d();

    private void push(){
        currentPath = nextPath;
        nextPath = null;
        var snapShot = currentPath;
        if(snapShot == null)return;
        cutoff = snapShot.length();
        travelled = 0;
    }

    public void setNextPath(@Nullable LerpPathV2<Vector3dc> nextPath){
        this.nextPath = nextPath;
        if(currentPath == null){
            push();
        }
    }

    public double remain(){
        var snapShot = nextPath;
        double d = snapShot == null ? 0 : snapShot.length();
        return cutoff - travelled + d;
    }

    public @Nullable Vector3dc peek(double distance){
        if(distance < travelled - 1e-4){
            return null;
        }
        var snapShot = currentPath;
        if(snapShot == null)return null;
        double nextPoint = distance;
        Vector3dc nextPointVector = snapShot.point(nextPoint);
        if(nextPoint < cutoff){
            return nextPointVector;
        }
        var _snapShot = nextPath;
        if (_snapShot == null)return nextPointVector;
        double _nextPoint = nextPoint - cutoff;
        return _snapShot.point(_nextPoint);
    }

    private @Nullable Vector3dc peekUncut(double distance){
        var snapShot = currentPath;
        if(snapShot == null){
            ControlCraft.LOGGER.warn("current path is null, returning null");
            return null;
        }
        double nextPoint = distance;
        Vector3dc nextPointVector = snapShot.point(nextPoint);
        if(nextPoint < snapShot.length()){
            return nextPointVector;
        }
        var _snapShot = nextPath;
        if (_snapShot == null)return nextPointVector;
        double _nextPoint = nextPoint - snapShot.length();
        return _snapShot.point(_nextPoint);
    }

    public @NotNull Vector3dc peekPrev(){
        return orElse(peek(travelled));
    }

    public @NotNull Vector3dc peekCurrent(){
        return orElse(peek(travelled + speed / 60));
    }

    public @NotNull Vector3dc peekNext(){
        return orElse(peek(travelled + 2 * speed / 60));
    }

    public void next(){
        double snap = travelled;
        travelled = snap + speed / 60;
        if(travelled > cutoff){
            push();
        }
    }


    private @NotNull Vector3dc orElse(Vector3dc nullable){
        if(nullable == null){
            ControlCraft.LOGGER.info("null encountered");
            return latestNonNull;
        }
        return new Vector3d(latestNonNull.set(nullable));
    }

    public void setSpeed(double speed){
        if(speed < 0)return;
        this.speed = speed;
    }

    public void cutoffIn(double delta){
        if(delta < 0)return;
        var snapShot = currentPath;
        if(snapShot == null)return;
        cutoff = Math.min(Math.min(travelled + delta, snapShot.length()), cutoff);
    }

    public @Nullable Vector3dc cutoffGoal(){
        return peekUncut(cutoff);
    }

    public @Nullable Vector3dc cutoffDirection(){
        Vector3dc p0 = peekUncut(cutoff);
        Vector3dc p1 = peekUncut(cutoff - speed / 60);
        if(p0 == null || p1 == null)return null;
        Vector3dc po = MathUtils.safeNormalize(p0.sub(p1, new Vector3d()));
        return po.length() < 1e-8 ? null : po;

    }

}

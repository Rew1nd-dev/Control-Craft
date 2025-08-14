package com.verr1.controlcraft.unstable.pathing.path;

import com.google.common.util.concurrent.AtomicDouble;
import com.verr1.controlcraft.unstable.util.BinarySearch;
import org.joml.Vector3dc;

import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;

public class CombinedPath implements IPath{

    private final List<IPath> subPath;
    private final List<Double> accumulatedDistance;
    private final double length;

    public CombinedPath(List<IPath> subPath) {
        if(subPath.isEmpty()){
            throw new IllegalArgumentException("SubPath cannot be empty");
        }
        this.subPath = subPath;
        this.length = subPath.stream().reduce(0.0, (acc, path) -> acc + path.length(), Double::sum);
        AtomicDouble acc = new AtomicDouble(0);
        this.accumulatedDistance = subPath.stream().map(path -> {
            double currentLength = path.length();
            double previousAccumulated = acc.get();
            acc.addAndGet(currentLength);
            return previousAccumulated + currentLength;
        }).toList();
    }

    // return the path index, this path contain this distance
    public int indexOf(double distance){
        return BinarySearch.maxIndexLessThan(accumulatedDistance, distance) + 1;
    }


    private int closestIndex(Vector3dc observe){
        return IntStream.range(0, subPath.size())
                .boxed()
                .min(Comparator.comparingDouble(i -> subPath.get(i).closestTo(observe).distanceSquared(observe)))
                .orElse(0);
    }

    @Override
    public Vector3dc closestTo(Vector3dc observe) {
        return subPath.stream()
                .map(path -> path.closestTo(observe))
                .min(Comparator.comparingDouble(v -> v.distanceSquared(observe)))
                .orElseGet(this::start);
    }

    @Override
    public double closestDistanceFromStart(Vector3dc observe) {
        int i = closestIndex(observe);
        return accumulatedDistance.get(i) - subPath.get(i).length() + subPath.get(i).closestDistanceFromStart(observe);
    }

    @Override
    public Vector3dc point(double distance) {
        if (distance < 0) {
            return start();
        }
        if(distance > length){
            return end();
        }
        int index = indexOf(distance);
        if (index < 0) {
            return start();
        }
        IPath path = subPath.get(index);
        double localDistance = distance - (index == 0 ? 0 : accumulatedDistance.get(index - 1));
        return path.point(localDistance);
    }

    @Override
    public double length() {
        return length;
    }

    @Override
    public Vector3dc end() {
        return subPath.get(subPath.size() - 1).end();
    }

    @Override
    public Vector3dc start() {
        return subPath.get(0).start();
    }
}

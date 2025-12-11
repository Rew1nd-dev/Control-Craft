package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.foundation.data.WorldBlockPos;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class CimulinkProfiler {

    private boolean started = true;




    private final Map<WorldBlockPos, MetricEntry> metrics = new ConcurrentHashMap<>();


    private WorldBlockPos tracked = null;
    private long trackedStartTime = 0;

    public void logMetric(WorldBlockPos pos, int value){
        metrics.computeIfAbsent(pos, p -> new MetricEntry()).log(value);
    }

    public double getAverageMetric(WorldBlockPos pos){
        MetricEntry entry = metrics.get(pos);
        if(entry == null) return 0;
        return entry.average();
    }

    public void start(){
        started = true;
    }

    public void stop(){
        started = false;
        tracked = null;
    }

    public void track(WorldBlockPos pos){
        if(!started) return;
        tracked = pos;
        trackedStartTime = System.nanoTime();
    }

    public void untrack(){
        if(!started || tracked == null) return;
        long duration = System.nanoTime() - trackedStartTime;
        logMetric(tracked, (int)(duration / 1_000_000));
        tracked = null;
    }


    public List<MetricQuery> queryMetrics(){
        return metrics.entrySet().stream()
                .map(e -> new MetricQuery(e.getKey(), e.getValue().average()))
                .toList();
    }

    public List<MetricQuery> queryTopMetrics(int count){
        return metrics.entrySet().stream()
                .map(e -> new MetricQuery(e.getKey(), e.getValue().average()))
                .sorted((a, b) -> Double.compare(b.average, a.average))
                .limit(count)
                .toList();
    }

    public record MetricQuery(WorldBlockPos pos, double average){

    }

    public static class MetricEntry{
        ArrayDeque<Integer> history = new ArrayDeque<>(10);

        public void log(int value){
            if(history.size() >= 10){
                history.removeFirst();
            }
            history.addLast(value);
        }

        public double average(){
            if(history.isEmpty()) return 0;
            int sum = 0;
            for(int v : history){
                sum += v;
            }
            return sum / (double)history.size();
        }

    }
}

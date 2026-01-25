package com.verr1.controlcraft.utils;

public class LazyTicker {

    private int lazyTickCounter = 0;
    private int lazyTickRate = 10;
    private Runnable task = () -> {};

    public LazyTicker(int lazyTickRate, Runnable task) {
        this.lazyTickRate = lazyTickRate;
        this.task = task;
    }

    public void tick(){
        if(lazyTickCounter-- > 0)return;
        lazyTickCounter = lazyTickRate;
        task.run();
    }
}

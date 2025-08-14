package com.verr1.controlcraft.unstable.util;

public class LazyRandom {

    private double latest;
    int tick;
    int coolDown = 10;

    public LazyRandom(int coolDown){
        this.coolDown = coolDown;
    }

    public double next(){
        if(tick -- < 0){
            tick = coolDown;
            latest = Math.random();
        }
        return peek();
    }

    public double peek(){
        return latest;
    }

}

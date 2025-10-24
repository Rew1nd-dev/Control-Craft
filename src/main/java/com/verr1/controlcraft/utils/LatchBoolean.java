package com.verr1.controlcraft.utils;

public class LatchBoolean {

    private boolean state;
    private int latchCounter;

    public void setLatchMax(int latchMax) {
        this.latchMax = latchMax;
    }

    private int latchMax;

    public LatchBoolean(boolean initialState, int latchMax) {
        this.state = initialState;
        this.latchMax = latchMax;
        this.latchCounter = 0;
    }

    public LatchBoolean(int latchMax) {
        this(false, latchMax);
    }

    public boolean peek(){
        return state;
    }

    public void latch(){
        state = true;
        latchCounter = latchMax;
    }

    public void tick(){
        if(latchCounter > 0){
            latchCounter--;
            if(latchCounter == 0){
                state = false;
            }
        }
    }

}

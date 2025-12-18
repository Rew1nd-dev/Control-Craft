package com.verr1.controlcraft.utils;

import java.util.function.Supplier;

public class Watcher {

    private int unpresentTime = 0;
    private final int time;
    final Runnable onUnpresent;
    final Supplier<Boolean> checker;

    private boolean alreadyUnpresent = true;

    public Watcher(
            Runnable onUnpresent,
            Supplier<Boolean> checker,
            int time
    ) {
        this.onUnpresent = onUnpresent;
        this.checker = checker;
        this.time = time;
    }


    public boolean check(){
        if(checker.get()){
            unpresentTime = 0;
            alreadyUnpresent = false;
        }else{
            unpresentTime++;
            if(unpresentTime >= time && !alreadyUnpresent){
                onUnpresent.run();
                alreadyUnpresent = true;
                return true;
            }
        }
        return false;
    }

    public void reset(){
        unpresentTime = 0;
        alreadyUnpresent = false;
    }

}

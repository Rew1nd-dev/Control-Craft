package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import org.luaj.vm2.LuaError;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.DebugLib;

public class WatcherLib extends DebugLib {

    private volatile boolean interrupted = false;

    @Override
    public void onInstruction(int i, Varargs varargs, int i1) {

        if(interrupted){
            interrupted = false;
            throw new LuaError("Execution interrupted");
        }


    }

    public void interrupt(){
        interrupted = true;
    }

}

package com.verr1.controlcraft.content.links.screen_base;

import com.verr1.controlcraft.content.links.screen_base.lua.libs.ComputerWatcherLib;
import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaError;

public class ComputerCommonLua {

    protected final Globals luaGlobals;

    public ComputerCommonLua(Globals luaGlobals) {
        this.luaGlobals = luaGlobals;
    }

    protected void runWithProtection(Runnable luaTask) {
        // 从 Globals 里提取我们刚塞进去的 ScreenWatcherLib 实例
        ComputerWatcherLib watcher = (ComputerWatcherLib) luaGlobals.debuglib;

        if (watcher != null) {
            // 设置当前这次能跑多久、多少步
            watcher.setLimits(10000, 2, 10 * 1024);
            // 必须在开始前重置计数器！
            watcher.resetForNewExecution();
        }

        try {
            // 执行目标代码，例如: loopFunction.call()
            luaTask.run();
        } catch (LuaError e) {
            // 如果捕获到超时，可以在控制台打印或者将这台显示器标记为错误状态
            onError(e);
        }
    }

    protected void onError(LuaError e){

    }

}

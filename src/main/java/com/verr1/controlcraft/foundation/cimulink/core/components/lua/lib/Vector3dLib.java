package com.verr1.controlcraft.foundation.cimulink.core.components.lua.lib;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.ThreeArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;

/**
 * 注册全局构造函数 Vector3d(x,y,z)
 */
public class Vector3dLib extends TwoArgFunction {
    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        env.set("Vector3d", new Vector3dConstructor());
        return LuaValue.NIL;
    }

    private static class Vector3dConstructor extends ThreeArgFunction {
        @Override
        public LuaValue call(LuaValue x, LuaValue y, LuaValue z) {
            double vx = x.optdouble(0.0);
            double vy = y.optdouble(0.0);
            double vz = z.optdouble(0.0);

            return new Vector3dValue(vx, vy, vz);
        }

        // overload factory for three args if needed via call(Varargs) can be added
    }
}

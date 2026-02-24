// java
package com.verr1.controlcraft.foundation.cimulink.core.components.lua;

import com.verr1.controlcraft.foundation.cimulink.core.api.IPhysAccess;
import org.jetbrains.annotations.NotNull;
import org.joml.Vector3dc;
import org.luaj.vm2.*;
import org.luaj.vm2.lib.*;

public class PhysLib extends TwoArgFunction {
    private final IPhysAccess source;

    public PhysLib(@NotNull IPhysAccess source) {
        this.source = source;
    }

    // 注册库：在 globals 中创建全局表 "Phys"
    @Override
    public LuaValue call(LuaValue modname, LuaValue env) {
        LuaTable phys = new LuaTable();

        phys.set("position", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return toLuaVec(source.position(), env);
            }
        });

        phys.set("velocity", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return toLuaVec(source.velocity(), env);
            }
        });

        phys.set("angularVelocity", new ZeroArgFunction() {
            @Override
            public LuaValue call() {return toLuaVec(source.angularVelocity(), env);
            }
        });

        phys.set("quaternionToWorld", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return toLuaQuat(source.quaternionToWorld(), env);
            }
        });

        phys.set("mass", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(source.mass());
            }
        });

        phys.set("inertia", new ZeroArgFunction() {
            @Override
            public LuaValue call() {
                return LuaValue.valueOf(source.inertia());
            }
        });

        env.set("Phys", phys);
        env.get("package").get("loaded").set("Phys", phys);
        return phys;
    }

    // 把 JOML Vector3dc 转为 Lua 对象：
    // 1) 如果 globals 中存在全局函数 Vector3d，则调用它：Vector3d(x,y,z)
    // 2) 否则返回一个普通 table { x=..., y=..., z=... }
    // java
    private static LuaValue toLuaVec(Vector3dc v, LuaValue env) {
        if (v == null) return LuaValue.NIL;
        LuaValue vec = env.get("Vector3d");
        if (!vec.isnil()) {
            // 优先支持 Vector3d:new(x,y,z)
            LuaValue ctorMethod = vec.get("new");
            if (ctorMethod.isfunction()) {
                return ctorMethod.invoke(LuaValue.varargsOf(new LuaValue[]{
                        vec,
                        LuaValue.valueOf(v.x()),
                        LuaValue.valueOf(v.y()),
                        LuaValue.valueOf(v.z())
                })).arg1();
            }
            // 退回到 Vector3d(x,y,z)
            if (vec.isfunction()) {
                return vec.call(
                        LuaValue.valueOf(v.x()),
                        LuaValue.valueOf(v.y()),
                        LuaValue.valueOf(v.z()));
            }
        }
        // 最后退回到普通 table { x=..., y=..., z=... }
        LuaTable t = new LuaTable();
        t.set("x", LuaValue.valueOf(v.x()));
        t.set("y", LuaValue.valueOf(v.y()));
        t.set("z", LuaValue.valueOf(v.z()));
        return t;
    }

    private static LuaValue toLuaQuat(org.joml.Quaterniondc q, LuaValue env) {
        if (q == null) return LuaValue.NIL;
        LuaValue quat = env.get("Quaterniond");
        if (!quat.isnil()) {
            // 优先支持 Quaterniond:new(x,y,z,w)
            LuaValue ctorMethod = quat.get("new");
            if (ctorMethod.isfunction()) {
                return ctorMethod.invoke(LuaValue.varargsOf(new LuaValue[]{
                        quat,
                        LuaValue.valueOf(q.x()),
                        LuaValue.valueOf(q.y()),
                        LuaValue.valueOf(q.z()),
                        LuaValue.valueOf(q.w())
                })).arg1();
            }
            // 退回到 Quaterniond(x,y,z,w)
            if (quat.isfunction()) {
                return quat.invoke(LuaValue.varargsOf(new LuaValue[]{
                        quat,
                        LuaValue.valueOf(q.x()),
                        LuaValue.valueOf(q.y()),
                        LuaValue.valueOf(q.z()),
                        LuaValue.valueOf(q.w())
                })).arg1();
            }
        }
        // 最后退回到普通 table { x=..., y=..., z=..., w=... }
        LuaTable t = new LuaTable();
        t.set("x", LuaValue.valueOf(q.x()));
        t.set("y", LuaValue.valueOf(q.y()));
        t.set("z", LuaValue.valueOf(q.z()));
        t.set("w", LuaValue.valueOf(q.w()));
        return t;
    }

}

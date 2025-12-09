
package com.verr1.controlcraft.foundation.cimulink.core.components.lua.lib;

import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.LuaUserdata;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.LuaTable;


public class Vector3dValue extends LuaUserdata {
    public double x, y, z;
    private static final LuaTable methods = new LuaTable();
    private static final LuaTable metatable = new LuaTable();

    static {
        // 方法：length (self)
        methods.set("length", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue self) {
                Vector3dValue v = expectVector(self, "self");
                return LuaValue.valueOf(Math.sqrt(v.x*v.x + v.y*v.y + v.z*v.z));
            }
        });

        // normalize (self)
        methods.set("normalize", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue self) {
                Vector3dValue v = expectVector(self, "self");
                double len = Math.sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
                if (len == 0) return new Vector3dValue(0, 0, 0);
                return new Vector3dValue(v.x / len, v.y / len, v.z / len);
            }
        });

        // dot(self, other)
        methods.set("dot", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue self, LuaValue other) {
                Vector3dValue a = expectVector(self, "self");
                Vector3dValue b = expectVector(other, "other");
                return LuaValue.valueOf(a.x*b.x + a.y*b.y + a.z*b.z);
            }
        });

        // cross(self, other)
        methods.set("cross", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue self, LuaValue other) {
                Vector3dValue a = expectVector(self, "self");
                Vector3dValue b = expectVector(other, "other");
                return new Vector3dValue(
                        a.y * b.z - a.z * b.y,
                        a.z * b.x - a.x * b.z,
                        a.x * b.y - a.y * b.x
                );
            }
        });

        // 元方法：__add
        metatable.set("__add", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue a, LuaValue b) {
                Vector3dValue va = expectVector(a, "operand1");
                Vector3dValue vb = expectVector(b, "operand2");
                return new Vector3dValue(va.x + vb.x, va.y + vb.y, va.z + vb.z);
            }
        });

        // __sub
        metatable.set("__sub", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue a, LuaValue b) {
                Vector3dValue va = expectVector(a, "operand1");
                Vector3dValue vb = expectVector(b, "operand2");
                return new Vector3dValue(va.x - vb.x, va.y - vb.y, va.z - vb.z);
            }
        });

        // __mul supports scalar*vector or vector*scalar or vector*vector (component-wise)
        metatable.set("__mul", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue a, LuaValue b) {
                if (a != null && a.isnumber()) {
                    double s = expectNumber(a, "operand1");
                    Vector3dValue v = expectVector(b, "operand2");
                    return new Vector3dValue(s*v.x, s*v.y, s*v.z);
                } else if (b != null && b.isnumber()) {
                    double s = expectNumber(b, "operand2");
                    Vector3dValue v = expectVector(a, "operand1");
                    return new Vector3dValue(s*v.x, s*v.y, s*v.z);
                } else {
                    Vector3dValue va = expectVector(a, "operand1");
                    Vector3dValue vb = expectVector(b, "operand2");
                    return new Vector3dValue(va.x*vb.x, va.y*vb.y, va.z*vb.z);
                }
            }
        });

        // __tostring
        metatable.set("__tostring", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue self) {
                Vector3dValue v = expectVector(self, "self");
                return LuaValue.valueOf(String.format("Vector3d(%.6f, %.6f, %.6f)", v.x, v.y, v.z));
            }
        });

        // __index: 返回字段或方法
        metatable.set("__index", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue self, LuaValue key) {
                String k = key.checkjstring();
                Vector3dValue v = expectVector(self, "self");
                switch (k) {
                    case "x": return LuaValue.valueOf(v.x);
                    case "y": return LuaValue.valueOf(v.y);
                    case "z": return LuaValue.valueOf(v.z);
                    default:
                        LuaValue m = methods.get(k);
                        return m.isnil() ? LuaValue.NIL : m;
                }
            }
        });

        // __newindex: 允许设置 x,y,z
        metatable.set("__newindex", new ThreeArgNewIndex());
    }

    // __newindex 的实现（单独类避免嵌套匿名类太长）
    private static class ThreeArgNewIndex extends VarArgFunction {
        @Override
        public Varargs invoke(Varargs args) {
            Vector3dValue v = expectVector(args.arg1(), "self");
            String key = args.arg(2).checkjstring();
            double val = args.arg(3).optdouble(0.0);
            switch (key) {
                case "x": v.x = val; break;
                case "y": v.y = val; break;
                case "z": v.z = val; break;
                default: // ignore
            }
            return LuaValue.NIL;
        }
    }

    public Vector3dValue(double x, double y, double z) {
        super(null); // userdata value not needed; we'll keep fields directly
        this.x = x;
        this.y = y;
        this.z = z;
        setmetatable(metatable);
    }

    // 方便的工厂（可在 LuaLib 构造器中调用）
    public static Vector3dValue of(double x, double y, double z) {
        return new Vector3dValue(x, y, z);
    }

    // --- 类型检查辅助方法 ---
    private static Vector3dValue expectVector(LuaValue v, String name) {
        if (v == null || v.isnil()) {
            throw new LuaError(name + " expected Vector3d, got nil");
        }
        try {
            // checkuserdata 会在类型不匹配时抛出 LuaError
            return (Vector3dValue) v.checkuserdata(Vector3dValue.class);
        } catch (LuaError e) {
            // 给出更友好的错误信息
            throw new LuaError(name + " expected Vector3d");
        } catch (ClassCastException e) {
            throw new LuaError(name + " expected Vector3d");
        }
    }

    private static double expectNumber(LuaValue v, String name) {
        if (v == null || v.isnil() || !v.isnumber()) {
            throw new LuaError(name + " expected number");
        }
        return v.todouble();
    }
}

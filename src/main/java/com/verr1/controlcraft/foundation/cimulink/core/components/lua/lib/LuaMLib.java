package com.verr1.controlcraft.foundation.cimulink.core.components.lua.lib;

import org.luaj.vm2.LuaError;
import org.luaj.vm2.LuaTable;
import org.luaj.vm2.LuaUserdata;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.Varargs;
import org.luaj.vm2.lib.OneArgFunction;
import org.luaj.vm2.lib.ThreeArgFunction;
import org.luaj.vm2.lib.TwoArgFunction;
import org.luaj.vm2.lib.VarArgFunction;

class Vector3dValue extends LuaUserdata {
    public double x, y, z;
    private static final LuaTable methods = new LuaTable();
    private static final LuaTable metatable = new LuaTable();

    static {
        metatable.set(LuaValue.INDEX, methods);

        // 方法：set (self, x, y, z) 或 set(self, other)
        methods.set("set", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                Vector3dValue self = expectVector3d(args.arg1(), "self");
                if (args.narg() == 2) {  // set(other)
                    Vector3dValue other = expectVector3d(args.arg(2), "other");
                    self.x = other.x;
                    self.y = other.y;
                    self.z = other.z;
                } else if (args.narg() >= 4) {  // set(x, y, z)
                    self.x = args.arg(2).checkdouble();
                    self.y = args.arg(3).checkdouble();
                    self.z = args.arg(4).checkdouble();
                } else {
                    throw new LuaError("set expects Vector3d or three numbers");
                }
                return self;
            }
        });

        // 方法：add (self, other) 或 add(self, dx, dy, dz)
        methods.set("add", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                Vector3dValue self = expectVector3d(args.arg1(), "self");
                Vector3dValue result = new Vector3dValue(self.x, self.y, self.z);
                if (args.narg() == 2) {  // add(other)
                    Vector3dValue other = expectVector3d(args.arg(2), "other");
                    result.x += other.x;
                    result.y += other.y;
                    result.z += other.z;
                } else if (args.narg() >= 4) {  // add(dx, dy, dz)
                    result.x += args.arg(2).checkdouble();
                    result.y += args.arg(3).checkdouble();
                    result.z += args.arg(4).checkdouble();
                } else {
                    throw new LuaError("add expects Vector3d or three numbers");
                }
                return result;
            }
        });

        // 方法：sub (self, other)
        methods.set("sub", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                return new Vector3dValue(self.x - other.x, self.y - other.y, self.z - other.z);
            }
        });

        // 方法：mul (self, other) 或 mul(self, scalar)
        methods.set("mul", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue arg) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                if (arg.isuserdata(Vector3dValue.class)) {
                    Vector3dValue other = (Vector3dValue)arg;
                    return new Vector3dValue(self.x * other.x, self.y * other.y, self.z * other.z);
                } else {
                    double scalar = arg.checkdouble();
                    return new Vector3dValue(self.x * scalar, self.y * scalar, self.z * scalar);
                }
            }
        });

        // 方法：div (self, other) 或 div(self, scalar)
        methods.set("div", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue arg) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                if (arg.isuserdata(Vector3dValue.class)) {
                    Vector3dValue other = (Vector3dValue)arg;
                    return new Vector3dValue(self.x / other.x, self.y / other.y, self.z / other.z);
                } else {
                    double scalar = arg.checkdouble();
                    return new Vector3dValue(self.x / scalar, self.y / scalar, self.z / scalar);
                }
            }
        });

        // 方法：length (self)
        methods.set("length", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                return LuaValue.valueOf(Math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z));
            }
        });

        // 方法：lengthSquared (self)
        methods.set("lengthSquared", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                return LuaValue.valueOf(self.x * self.x + self.y * self.y + self.z * self.z);
            }
        });

        // 方法：normalize (self)
        methods.set("normalize", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                double len = Math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z);
                if (len == 0) return new Vector3dValue(self.x, self.y, self.z);
                return new Vector3dValue(self.x / len, self.y / len, self.z / len);
            }
        });

        // 方法：dot (self, other)
        methods.set("dot", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                return LuaValue.valueOf(self.x * other.x + self.y * other.y + self.z * other.z);
            }
        });

        // 方法：cross (self, other)
        methods.set("cross", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                return new Vector3dValue(
                        self.y * other.z - self.z * other.y,
                        self.z * other.x - self.x * other.z,
                        self.x * other.y - self.y * other.x
                );
            }
        });

        // 方法：distance (self, other)
        methods.set("distance", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                double dx = self.x - other.x;
                double dy = self.y - other.y;
                double dz = self.z - other.z;
                return LuaValue.valueOf(Math.sqrt(dx * dx + dy * dy + dz * dz));
            }
        });

        // 方法：angle (self, other)
        methods.set("angle", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                double dot = self.x * other.x + self.y * other.y + self.z * other.z;
                double len = Math.sqrt((self.x * self.x + self.y * self.y + self.z * self.z) * (other.x * other.x + other.y * other.y + other.z * other.z));
                if (len == 0) return LuaValue.valueOf(0);
                return LuaValue.valueOf(Math.acos(dot / len));
            }
        });

        // 方法：negate (self)
        methods.set("negate", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                return new Vector3dValue(-self.x, -self.y, -self.z);
            }
        });

        // 方法：absolute (self)
        methods.set("absolute", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                return new Vector3dValue(Math.abs(self.x), Math.abs(self.y), Math.abs(self.z));
            }
        });

        // 方法：lerp (self, other, t)
        methods.set("lerp", new ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal, LuaValue tVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                double t = tVal.checkdouble();
                return new Vector3dValue(
                        self.x + (other.x - self.x) * t,
                        self.y + (other.y - self.y) * t,
                        self.z + (other.z - self.z) * t
                );
            }
        });

        // 方法：min (self, other)
        methods.set("min", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                return new Vector3dValue(
                        Math.min(self.x, other.x),
                        Math.min(self.y, other.y),
                        Math.min(self.z, other.z)
                );
            }
        });

        // 方法：max (self, other)
        methods.set("max", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                Vector3dValue self = expectVector3d(selfVal, "self");
                Vector3dValue other = expectVector3d(otherVal, "other");
                return new Vector3dValue(
                        Math.max(self.x, other.x),
                        Math.max(self.y, other.y),
                        Math.max(self.z, other.z)
                );
            }
        });
    }

    public Vector3dValue(double x, double y, double z) {
        super(new Object(), metatable);
        this.x = x;
        this.y = y;
        this.z = z;
    }

    // 类型检查
    public static Vector3dValue expectVector3d(LuaValue value, String message) {
        if (!value.isuserdata(Vector3dValue.class)) {
            throw new LuaError(message);
        }
        return (Vector3dValue)value;
    }
}

class QuaterniondValue extends LuaUserdata {
    public double x, y, z, w;
    private static final LuaTable methods = new LuaTable();
    private static final LuaTable metatable = new LuaTable();


    static {
        metatable.set(LuaValue.INDEX, methods);

        // 方法：set (self, x, y, z, w) 或 set(self, other)
        methods.set("set", new VarArgFunction() {
            @Override
            public Varargs invoke(Varargs args) {
                QuaterniondValue self = expectQuaterniond(args.arg1(), "self");
                if (args.narg() == 2) {  // set(other)
                    QuaterniondValue other = expectQuaterniond(args.arg(2), "other");
                    self.x = other.x;
                    self.y = other.y;
                    self.z = other.z;
                    self.w = other.w;
                } else if (args.narg() >= 5) {  // set(x, y, z, w)
                    self.x = args.arg(2).checkdouble();
                    self.y = args.arg(3).checkdouble();
                    self.z = args.arg(4).checkdouble();
                    self.w = args.arg(5).checkdouble();
                } else {
                    throw new LuaError("set expects Quaterniond or four numbers");
                }
                return self;
            }
        });

        // 方法：setFromAxisAngle (self, axis, angle)
        methods.set("setFromAxisAngle", new ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue axisVal, LuaValue angleVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                Vector3dValue axis = Vector3dValue.expectVector3d(axisVal, "axis");
                double angle = angleVal.checkdouble();
                double halfAngle = angle / 2;
                double sin = Math.sin(halfAngle);
                double cos = Math.cos(halfAngle);
                self.x = axis.x * sin;
                self.y = axis.y * sin;
                self.z = axis.z * sin;
                self.w = cos;
                // Normalize self
                double len = Math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w);
                if (len != 0) {
                    self.x /= len;
                    self.y /= len;
                    self.z /= len;
                    self.w /= len;
                }
                return self;
            }
        });

        // 方法：add (self, other)
        methods.set("add", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                QuaterniondValue other = expectQuaterniond(otherVal, "other");
                return new QuaterniondValue(self.x + other.x, self.y + other.y, self.z + other.z, self.w + other.w);
            }
        });

        // 方法：mul (self, other) 或 mul(self, scalar)
        methods.set("mul", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue arg) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                if (arg.isuserdata(QuaterniondValue.class)) {
                    QuaterniondValue other = (QuaterniondValue)arg;
                    return new QuaterniondValue(
                            self.w * other.x + self.x * other.w + self.y * other.z - self.z * other.y,
                            self.w * other.y + self.y * other.w + self.z * other.x - self.x * other.z,
                            self.w * other.z + self.z * other.w + self.x * other.y - self.y * other.x,
                            self.w * other.w - self.x * other.x - self.y * other.y - self.z * other.z
                    );
                } else {
                    double scalar = arg.checkdouble();
                    return new QuaterniondValue(self.x * scalar, self.y * scalar, self.z * scalar, self.w * scalar);
                }
            }
        });

        // 方法：length (self)
        methods.set("length", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                return LuaValue.valueOf(Math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w));
            }
        });

        // 方法：normalize (self)
        methods.set("normalize", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                double len = Math.sqrt(self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w);
                if (len == 0) return new QuaterniondValue(self.x, self.y, self.z, self.w);
                return new QuaterniondValue(self.x / len, self.y / len, self.z / len, self.w / len);
            }
        });

        // 方法：conjugate (self)
        methods.set("conjugate", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                return new QuaterniondValue(-self.x, -self.y, -self.z, self.w);
            }
        });

        // 方法：inverse (self)
        methods.set("inverse", new OneArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                double lenSq = self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w;
                if (lenSq == 0) throw new LuaError("Cannot invert zero quaternion");
                double invLenSq = 1.0 / lenSq;
                return new QuaterniondValue(-self.x * invLenSq, -self.y * invLenSq, -self.z * invLenSq, self.w * invLenSq);
            }
        });

        // 方法：transform (self, vec)
        methods.set("transform", new TwoArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue vecVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                Vector3dValue vec = Vector3dValue.expectVector3d(vecVal, "vec");
                QuaterniondValue v = new QuaterniondValue(vec.x, vec.y, vec.z, 0);
                QuaterniondValue conj = new QuaterniondValue(-self.x, -self.y, -self.z, self.w);
                QuaterniondValue res = quaternionMul(self, quaternionMul(v, conj));
                return new Vector3dValue(res.x, res.y, res.z);
            }

            private QuaterniondValue quaternionMul(QuaterniondValue a, QuaterniondValue b) {
                return new QuaterniondValue(
                        a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y,
                        a.w * b.y + a.y * b.w + a.z * b.x - a.x * b.z,
                        a.w * b.z + a.z * b.w + a.x * b.y - a.y * b.x,
                        a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z
                );
            }
        });

        // 方法：slerp (self, other, t)
        methods.set("slerp", new ThreeArgFunction() {
            @Override
            public LuaValue call(LuaValue selfVal, LuaValue otherVal, LuaValue tVal) {
                QuaterniondValue self = expectQuaterniond(selfVal, "self");
                QuaterniondValue other = expectQuaterniond(otherVal, "other");
                double t = tVal.checkdouble();
                double dot = self.x * other.x + self.y * other.y + self.z * other.z + self.w * other.w;
                boolean negate = dot < 0;
                if (negate) dot = -dot;
                QuaterniondValue o = negate ? new QuaterniondValue(-other.x, -other.y, -other.z, -other.w) : other;
                if (dot > 0.9995) {
                    // Linear interpolation
                    QuaterniondValue result = new QuaterniondValue(
                            self.x + t * (o.x - self.x),
                            self.y + t * (o.y - self.y),
                            self.z + t * (o.z - self.z),
                            self.w + t * (o.w - self.w)
                    );
                    double len = Math.sqrt(result.x * result.x + result.y * result.y + result.z * result.z + result.w * result.w);
                    if (len != 0) {
                        result.x /= len;
                        result.y /= len;
                        result.z /= len;
                        result.w /= len;
                    }
                    return result;
                }
                double theta = Math.acos(dot);
                double sinTheta = Math.sin(theta);
                double scale0 = Math.sin((1 - t) * theta) / sinTheta;
                double scale1 = Math.sin(t * theta) / sinTheta;
                return new QuaterniondValue(
                        self.x * scale0 + o.x * scale1,
                        self.y * scale0 + o.y * scale1,
                        self.z * scale0 + o.z * scale1,
                        self.w * scale0 + o.w * scale1
                );
            }
        });
    }

    public QuaterniondValue(double x, double y, double z, double w) {
        super(new Object(), metatable);
        this.x = x;
        this.y = y;
        this.z = z;
        this.w = w;
    }

    // 类型检查
    public static QuaterniondValue expectQuaterniond(LuaValue value, String message) {
        if (!value.isuserdata(QuaterniondValue.class)) {
            throw new LuaError(message);
        }
        return (QuaterniondValue)value;
    }
}

class Vector3dLib extends TwoArgFunction {
    @Override
    public LuaValue call(LuaValue modName, LuaValue env) {
        LuaTable vector3d = new LuaTable();
        vector3d.set("new", new Vector3dConstructor());
        env.set("Vector3d", vector3d);
        return LuaValue.NIL;
    }

    private static class Vector3dConstructor extends VarArgFunction {
        @Override
        public Varargs invoke(Varargs args) {
            double x = args.optdouble(1, 0.0);
            double y = args.optdouble(2, 0.0);
            double z = args.optdouble(3, 0.0);
            return new Vector3dValue(x, y, z);
        }
    }
}

class QuaterniondLib extends TwoArgFunction {
    @Override
    public LuaValue call(LuaValue modName, LuaValue env) {
        LuaTable quaterniond = new LuaTable();
        quaterniond.set("new", new QuaterniondConstructor());
        env.set("Quaterniond", quaterniond);
        return LuaValue.NIL;
    }

    private static class QuaterniondConstructor extends VarArgFunction {
        @Override
        public Varargs invoke(Varargs args) {
            double x = args.optdouble(1, 0.0);
            double y = args.optdouble(2, 0.0);
            double z = args.optdouble(3, 0.0);
            double w = args.optdouble(4, 1.0);
            return new QuaterniondValue(x, y, z, w);
        }

    }
}

// 使用示例：在 Globals 中加载
// globals.load(new Vector3dLib());
// globals.load(new QuaterniondLib());
// 在 Lua: local v = Vector3d:new(1,2,3)
// local q = Quaterniond:new(0,0,0,1)
// local len = v:length()
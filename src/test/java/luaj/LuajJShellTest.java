package luaj;

import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.jse.JsePlatform;

public class LuajJShellTest {

    public static void main(String[] args) {
        // 1. 创建 Luaj 的 JSE 标准 Globals（自动包含 luajava 库）
        Globals globals = JsePlatform.standardGlobals();

        // Lua 脚本内容
        String luaScript =
            "local JShell = luajava.bindClass('jdk.jshell.JShell')\n" +
                "local jshell = JShell:create()\n" +
                "\n" +
                "-- 执行打印 Hello World\n" +
                "local events = jshell:eval('System.out.println(\"Hello World from JShell via Lua!\")')\n" +
                "jshell:close()"+
                "\n" +
                "print('JShell eval 执行成功！')\n";

        try {
            System.out.println("=== 开始执行 Lua 脚本 ===");

            // 加载并执行 Lua 脚本
            LuaValue chunk = globals.load(luaScript, "jshell_test.lua");
            chunk.call();

            System.out.println("=== Lua 脚本执行结束 ===");

        } catch (Exception e) {
            System.err.println("执行失败：");
            e.printStackTrace();
        }
    }
}
package luaj;

import org.luaj.vm2.Globals;
import org.luaj.vm2.LuaValue;
import org.luaj.vm2.lib.jse.JsePlatform;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Path;

public class Test {

    public static void main(String[] args) {
        Globals globals = JsePlatform.standardGlobals();
        LuaValue chunk = globals.load("print('Hello from Lua!')");
        chunk.call();
    }


}

package luaj;

import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitConstructor;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class LuacuitTest {

    public static void main(String[] args) {
        String path = System.getProperty("user.dir") + "\\src\\test\\java\\luaj\\LuacuitTester.lua";
        String luaScript = loadLua(path);

        LuacuitConstructor constructor = new LuacuitConstructor(luaScript);
        Luacuit luacuit = constructor.build();

        List<String> inNames = luacuit.namedInputs().keySet().stream().toList();
        List<String> outNames = luacuit.namedOutputs().keySet().stream().toList();

        luacuit.input(inNames.get(0), 5.0).input(inNames.get(1), 3.0).input(inNames.get(2), 1.0);
        for(int i = 0; i < 10; i++){
            try{
                luacuit.onPositiveEdge();
            }catch (Exception e){
                Luacuit.close();
                return;
            }
            double output  = luacuit.peekOutput(outNames.get(0));
            double output2 = luacuit.peekOutput(outNames.get(1));
            double output3 = luacuit.peekOutput(outNames.get(1));

            System.out.println(output );
            System.out.println(output2);
            System.out.println(output3);
            System.out.println("----");
        }

        Luacuit.close();
        System.out.println("success");
        return;
    }

    public static String loadLua(String path){
        StringBuilder content = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(path))) {
            String line;
            while ((line = br.readLine()) != null) {
                content.append(line).append("\n");
            }
            return content.toString();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return path;
    }

}

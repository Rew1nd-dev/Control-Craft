package luaj;

import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.Luacuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.luacuit.LuacuitScript;

public class LuacuitTest2 {
    public static void main(String[] args) {
        LuacuitScript ls = LuacuitScript.fromCode(LuacuitScript.EMPTY_CODE);
        Luacuit lc = new LuacuitConstructor(LuacuitScript.EMPTY_CODE).build();
    }

}

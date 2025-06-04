package test.game;

import com.verr1.controlcraft.foundation.cimulink.game.debug.Debug;
import com.verr1.controlcraft.foundation.cimulink.game.port.BlockLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.FFLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.digital.GateLinkPort;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.FFTypes;
import com.verr1.controlcraft.foundation.cimulink.game.port.types.GateTypes;

public class BlockLinkPortTest {


    public static void connectivityTest(){
        var or0 = new GateLinkPort(Debug.MapToDebug(0));
        var or1 = new GateLinkPort(Debug.MapToDebug(1));
        var or2 = new GateLinkPort(Debug.MapToDebug(2));

        // TestEnvBlockLinkWorld.add(or0, or1, or2);

        or0.setCurrentType(GateTypes.OR);
        or1.setCurrentType(GateTypes.AND);
        or2.setCurrentType(GateTypes.OR);


        or0.connectTo(or0.out(0), Debug.MapToDebug(1), or1.in(0));
        or1.connectTo(or1.out(0), Debug.MapToDebug(2), or2.in(0));

        or0.input(or0.in(0), 1.0);
        or1.input(or1.in(1), 1.0);

        BlockLinkPort.propagateCombinational(new BlockLinkPort.PropagateContext(), or0);

        System.out.println(or2.output(or2.out(0)));

    }

    public static void loopTest(){
        int n = 5;
        FFLinkPort[] ffs = new FFLinkPort[n];
        for(int i = 0; i < ffs.length; i++){
            ffs[i] = new FFLinkPort(Debug.MapToDebug(i));
            ffs[i].setCurrentType(FFTypes.D_FF);
            // TestEnvBlockLinkWorld.add(ffs[i]);
            if(i == 0)continue;
            ffs[i - 1].connectTo(ffs[i - 1].out(0), ffs[i].pos(), ffs[i].in(0));
        }
        ffs[n - 1].connectTo(ffs[n - 1].out(0), ffs[0].pos(), ffs[0].in(0));

        ffs[0].input(ffs[0].in(0), 1.0);


        for(int j = 0; j < 10; j++){
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < ffs.length; i++){
                sb.append(i).append(": ").append(ffs[i].output(ffs[i].out(0))).append("|");
            }
            System.out.println(sb);
            BlockLinkPort.propagateTemporal();
        }




    }


}

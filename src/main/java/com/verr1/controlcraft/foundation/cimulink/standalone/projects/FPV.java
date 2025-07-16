package com.verr1.controlcraft.foundation.cimulink.standalone.projects;

import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.LinearAdder;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.Circuit;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitDebugger;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.DirectCurrent;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;

import java.util.List;

public class FPV {
    public static CircuitNbt fpv_decomposition(){
        LinearAdder[] decomposition = new LinearAdder[]{
                new LinearAdder(List.of(0.25, -0.25, 0.25, 0.25)),
                new LinearAdder(List.of(0.25, 0.25, -0.25, 0.25)),
                new LinearAdder(List.of(0.25, 0.25, 0.25, -0.25)),
                new LinearAdder(List.of(0.25, -0.25, -0.25, -0.25))
        };
        CircuitConstructor constructor = new CircuitConstructor();
        constructor.addComponent(decomposition);
        constructor
                .defineInput("Fy", decomposition[0].__in(0), decomposition[1].__in(0), decomposition[2].__in(0), decomposition[3].__in(0))
                .defineInput("Tx", decomposition[0].__in(1), decomposition[1].__in(1), decomposition[2].__in(1), decomposition[3].__in(1))
                .defineInput("Ty", decomposition[0].__in(2), decomposition[1].__in(2), decomposition[2].__in(2), decomposition[3].__in(2))
                .defineInput("Tz", decomposition[0].__in(3), decomposition[1].__in(3), decomposition[2].__in(3), decomposition[3].__in(3));

        constructor
                .defineOutput("w1", decomposition[0].__out(0))
                .defineOutput("w2", decomposition[1].__out(0))
                .defineOutput("w3", decomposition[2].__out(0))
                .defineOutput("w4", decomposition[3].__out(0));


        return constructor.buildContext();
    }

    public static CircuitNbt construct(){
        Circuit closeLoop = fpv_closeLoop().buildCircuit();
        Circuit input = fpv_input().buildCircuit();
        Circuit decomposition = fpv_decomposition().buildCircuit();

        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(closeLoop, input, decomposition);

        constructor
                .defineInput("pitch", input.__in("pitch"))
                .defineInput("yaw", input.__in("yaw"))
                .defineInput("roll", input.__in("roll"))
                .defineInput("thrust", input.__in("thrust"))
                .defineInput("G_pitch", input.__in("G_pitch"))
                .defineInput("G_yaw", input.__in("G_yaw"))
                .defineInput("G_roll", input.__in("G_roll"))
                .defineInput("G_thrust", input.__in("G_thrust"));

        constructor
                .defineInput("cwx", closeLoop.__in("cwx"))
                .defineInput("cwy", closeLoop.__in("cwy"))
                .defineInput("cwz", closeLoop.__in("cwz"));

        constructor
                .defineInput("P", closeLoop.__in("P"))
                .defineInput("D", closeLoop.__in("D"))
                .defineInput("Inertia", closeLoop.__in("Gc"));

        constructor
                .connect(input.__out("tx"), closeLoop.__in("twx"))
                .connect(input.__out("ty"), closeLoop.__in("twy"))
                .connect(input.__out("tz"), closeLoop.__in("twz"));

        constructor
                .connect(input.__out("fy"), decomposition.__in("Fy"))
                .connect(closeLoop.__out("tx"), decomposition.__in("Tx"))
                .connect(closeLoop.__out("ty"), decomposition.__in("Ty"))
                .connect(closeLoop.__out("tz"), decomposition.__in("Tz"));

        constructor
                .defineOutput("w1", decomposition.__out("w1"))
                .defineOutput("w2", decomposition.__out("w2"))
                .defineOutput("w3", decomposition.__out("w3"))
                .defineOutput("w4", decomposition.__out("w4"));

        return constructor.buildContext();

    }

    public static CircuitNbt fpv_input(){
        Functions.FunctionN[] pitch_yaw_roll = new Functions.FunctionN[]{
                Functions.POWER.get(),
                Functions.POWER.get(),
                Functions.POWER.get()
        };

        Functions.FunctionN[] g_pitch_yaw_roll = new Functions.FunctionN[]{
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2)
        };

        DirectCurrent pow = new DirectCurrent(1.5);
        Functions.FunctionN g_thrust = Functions.PRODUCT.apply(2);

        CircuitConstructor constructor = new CircuitConstructor();
        constructor
                .addComponent(pow, g_thrust)
                .addComponent(pitch_yaw_roll)
                .addComponent(g_pitch_yaw_roll);

        constructor
                .defineInput("pitch", pitch_yaw_roll[0].__in(0))
                .defineInput("yaw", pitch_yaw_roll[1].__in(0))
                .defineInput("roll", pitch_yaw_roll[2].__in(0))
                .connect(pow.__out(0), pitch_yaw_roll[0].__in(1), pitch_yaw_roll[1].__in(1), pitch_yaw_roll[2].__in(1))
                .defineInput("G_pitch", g_pitch_yaw_roll[0].__in(1))
                .defineInput("G_yaw", g_pitch_yaw_roll[1].__in(1))
                .defineInput("G_roll", g_pitch_yaw_roll[2].__in(1))
                .connect(pitch_yaw_roll[0].__out(0), g_pitch_yaw_roll[0].__in(0))
                .connect(pitch_yaw_roll[1].__out(0), g_pitch_yaw_roll[1].__in(0))
                .connect(pitch_yaw_roll[2].__out(0), g_pitch_yaw_roll[2].__in(0))
                .defineOutput("tx", g_pitch_yaw_roll[0].__out(0))
                .defineOutput("ty", g_pitch_yaw_roll[1].__out(0))
                .defineOutput("tz", g_pitch_yaw_roll[2].__out(0));

        constructor
                .defineInput("thrust", g_thrust.__in(0))
                .defineInput("G_thrust", g_thrust.__in(1))
                .defineOutput("fy", g_thrust.__out(0));


        return constructor.buildContext();
    }

    public static CircuitNbt fpv_closeLoop(){
        Functions.FunctionN[] g_tw = new Functions.FunctionN[]{
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2)
        };

        Functions.FunctionN[] g_cw = new Functions.FunctionN[]{
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2)
        };

        Functions.FunctionN[] g_torque = new Functions.FunctionN[]{
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2),
                Functions.PRODUCT.apply(2)
        };

        LinearAdder p_minus_d = (LinearAdder)new LinearAdder(List.of(1.0, -1.0)).withName("P-D");
        LinearAdder[] tw_minus_cw = new LinearAdder[]{
                (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("P+D"),
                (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("P+D"),
                (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("P+D")
        };

        CircuitConstructor constructor = new CircuitConstructor();
        constructor
                .addComponent(g_tw)
                .addComponent(g_cw)
                .addComponent(g_torque)
                .addComponent(tw_minus_cw)
                .addComponent(p_minus_d);

        constructor
                .defineInput("P", g_tw[0].__in(1))
                .defineInput("twx", g_tw[0].__in(0))
                .defineInput("P", g_tw[1].__in(1))
                .defineInput("twy", g_tw[1].__in(0))
                .defineInput("P", g_tw[2].__in(1))
                .defineInput("twz", g_tw[2].__in(0));

        constructor
                .defineInput("P", p_minus_d.__in(0))
                .defineInput("D", p_minus_d.__in(1));

        constructor
                .defineInput("cwx", g_cw[0].__in(0))
                .connect(p_minus_d.__out(0), g_cw[0].__in(1))
                .defineInput("cwy", g_cw[1].__in(0))
                .connect(p_minus_d.__out(0), g_cw[1].__in(1))
                .defineInput("cwz", g_cw[2].__in(0))
                .connect(p_minus_d.__out(0), g_cw[2].__in(1));

        constructor
                .connect(g_tw[0].__out(0), tw_minus_cw[0].__in(0))
                .connect(g_tw[1].__out(0), tw_minus_cw[1].__in(0))
                .connect(g_tw[2].__out(0), tw_minus_cw[2].__in(0))
                .connect(g_cw[0].__out(0), tw_minus_cw[0].__in(1))
                .connect(g_cw[1].__out(0), tw_minus_cw[1].__in(1))
                .connect(g_cw[2].__out(0), tw_minus_cw[2].__in(1));

        constructor
                .defineInput("Gc", g_torque[0].__in(1))
                .connect(tw_minus_cw[0].__out(0), g_torque[0].__in(0))
                .defineInput("Gc", g_torque[1].__in(1))
                .connect(tw_minus_cw[1].__out(0), g_torque[1].__in(0))
                .defineInput("Gc", g_torque[2].__in(1))
                .connect(tw_minus_cw[2].__out(0), g_torque[2].__in(0));

        constructor
                .defineOutput("tx", g_torque[0].__out(0))
                .defineOutput("ty", g_torque[1].__out(0))
                .defineOutput("tz", g_torque[2].__out(0));

        // Fy, Tx, Ty, Tz
        return constructor.buildContext();

    }


    public static void test(){

        Circuit closeLoop = fpv_closeLoop().buildCircuit();
        Circuit input = fpv_input().buildCircuit();
        Circuit decomposition = fpv_decomposition().buildCircuit();


        CircuitDebugger db_closeLoop = new CircuitDebugger(closeLoop);

        closeLoop.input("P", 0).input("D", 1).input("Gc", 1).input("cwx", 1).input("cwy", 1).input("cwz", 1);

        db_closeLoop.track(closeLoop.__out(0), closeLoop.__out(1), closeLoop.__out(2));
        db_closeLoop.trackWithPeriod(1, 1, 1);

        System.out.println("finish closeLoop test");

        CircuitDebugger db_input = new CircuitDebugger(input);
        input
                .input("G_pitch", 1).input("G_yaw", 1).input("G_roll", 1).input("G_thrust", 1)
                .input("thrust", 0)
                .input("pitch", 0)
                .input("yaw", 0)
                .input("roll", 1);

        db_input.track(input.__out(0), input.__out(1), input.__out(2), input.__out(3));
        db_input.trackWithPeriod(1, 1, 1);

    }

}

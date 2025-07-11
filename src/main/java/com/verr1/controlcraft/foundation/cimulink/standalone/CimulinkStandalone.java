package com.verr1.controlcraft.foundation.cimulink.standalone;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Functions;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.LinearAdder;
import com.verr1.controlcraft.foundation.cimulink.core.components.analog.Shifter;
import com.verr1.controlcraft.foundation.cimulink.core.components.circuit.CircuitConstructor;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.da.Multiplexer;
import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.List;

public class CimulinkStandalone {
    static String DataPath = System.getProperty("user.dir") + "\\src\\main\\resources\\data\\vscontrolcraft\\cimulinks\\";;

    public static void save(CircuitNbt nbt, Path folder, String saveName) {
        CompoundTag tag = new CompoundTag();
        tag.put("circuitNbt", nbt.serialize());
        tag.put("sel0", new CompoundTag());
        tag.put("sel1", new CompoundTag());

        Path file = folder.resolve(saveName + ".nbt").toAbsolutePath();
        try{
            Files.createDirectories(folder);
            try(OutputStream out = Files.newOutputStream(file, StandardOpenOption.CREATE)){
                NbtIo.writeCompressed(tag, out);
            }

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void demo_0(){

        LinearAdder fma_0 = (LinearAdder)new LinearAdder(List.of(1.0, 1.0)).withName("FMA_0");
        Shifter reg_0 = (Shifter)new Shifter(0, 1).withName("REG_0");
        NamedComponent max = Functions.MAX.apply(2).withName("MAX_0");
        NamedComponent min = Functions.MIN.apply(2).withName("MIN_0");
        Multiplexer mux_0 = (Multiplexer)new Multiplexer(1).withName("MUX_0");

        CircuitConstructor constructor = new CircuitConstructor();

        constructor.addComponent(fma_0, reg_0, max, min, mux_0);

        constructor
                .defineInput("inc", mux_0.__dat(0))
                .defineInput("dec", mux_0.__dat(1))
                .defineInput("i/d", mux_0.__sel(0))
                .defineInput("max", min.__in(1))
                .defineInput("min", max.__in(1))
                .defineOutput("sum", reg_0.__out(0))

                .connect(mux_0.__out(0), fma_0.__in(1))
                .connect(fma_0.__out(0), max.__in(0))
                .connect(max.__out(0), min.__in(0))
                .connect(min.__out(0), reg_0.__in(0))
                .connect(reg_0.__out(0), fma_0.__in(0));

        CircuitNbt context = constructor.buildContext();
        save(context, Path.of(DataPath), "range_adder");
/*
        Circuit circuit = constructor.build("range_adder");
        CircuitDebugger debugger = new CircuitDebugger(circuit);
        debugger.track(circuit.__out(0), mux_0.__out(0), fma_0.__out(0), max.__out(0));
        circuit.input("inc", 1.0);
        circuit.input("dec", -1.0);
        circuit.input("max", 15.0);
        circuit.input("min", 0.0);

        circuit.input("i/d", 0.0);
        debugger.trackWithPeriod(0.05, 0.05, 1);
        circuit.input("i/d", 1.0);
        debugger.trackWithPeriod(0.05, 0.05, 1);
* */

    }



    public static void main(String[] args) {
        demo_0();
    }

}

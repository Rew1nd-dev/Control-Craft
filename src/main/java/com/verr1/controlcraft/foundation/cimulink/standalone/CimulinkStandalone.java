package com.verr1.controlcraft.foundation.cimulink.standalone;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.projects.*;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

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


    public static void main(String[] args) {
        // DigitalCircuits.test();
        // save(DigitalCircuits.cycleAdder(), Path.of(DataPath), "cycleAdder");
        // save(DigitalCircuits.integralUnit(), Path.of(DataPath), "integralUnit");
        // Eval.attacker();
        // Eval.testSubModule();
        // Eval.test();
        // save(Jet.create(), Path.of(DataPath), "attacker");
        // save(WarThunderFlight.flight(), Path.of(DataPath), "yprFlight");
        // save(View.create(), Path.of(DataPath), "view");
        // save(DigitalCircuits.decoder8(), Path.of(DataPath), "decoder8");
        // save(WarThunderFlight.Sel(), Path.of(DataPath), "viewManualSel");
        // save(WtHeli.pd3(), Path.of(DataPath), "pd3");
        // save(Missile.deltaCoordinate(), Path.of(DataPath), "dir");
        // System.out.println(1 - Math.exp(-5 * 0.01667));
        // save(Missile.create(), Path.of(DataPath), "missile");
        // save(Missile.control().evaluate().buildContext(), Path.of(DataPath), "mControl");
        // save(FPV.create().evaluate().buildContext(), Path.of(DataPath), "fpv");
        save(AimPredict.create(), Path.of(DataPath), "aimPredict");
        // AimPredict.testAim();
        // Missile.testYP();
    }

}

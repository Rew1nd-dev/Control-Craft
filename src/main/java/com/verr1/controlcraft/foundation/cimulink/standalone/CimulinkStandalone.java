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
    static String DataPath = System.getProperty("user.dir") + "\\src\\main\\resources\\data\\vscontrolcraft\\cimulinks\\";; //".cimulinks";//

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
        // save(Flights.flight6(), Path.of(DataPath), "yprFlight4");
        // save(View.create(), Path.of(DataPath), "view");
        // save(DigitalCircuits.decoder8(), Path.of(DataPath), "decoder8");
        // save(WarThunderFlight.Sel(), Path.of(DataPath), "viewManualSel");
        // save(WtHeli.pd3(), Path.of(DataPath), "pd3");
        // save(Missile.deltaCoordinate().evaluate().buildContext(), Path.of(DataPath), "dir");
        // System.out.println(1 - Math.exp(-5 * 0.01667));
        // save(Missile.create(), Path.of(DataPath), "missile");
        // save(Missile.control().evaluate().buildContext(), Path.of(DataPath), "mControl");
        // save(FPV.create().evaluate().buildContext(), Path.of(DataPath), "fpv");
        // save(AimPredict.create(), Path.of(DataPath), "aimPredict");
        // save(JetMissile.closeLoopXYZ().evaluate().buildContext(), Path.of(DataPath), "jetMissile");
        // save(JetMissile.computeYP().evaluate().buildContext(), Path.of(DataPath), "cyp");

        save(CCRP.cannon8().evaluate().buildContext(), Path.of(DataPath), "cannon8");

        // save(WarThunderFlight.lerp().evaluate().buildContext(), Path.of(DataPath), "testLoop");
        // System.out.println(Math.pow(1.001, -0.001));
        // AimPredict.testAim();
        // Missile.testYP();
    }

}

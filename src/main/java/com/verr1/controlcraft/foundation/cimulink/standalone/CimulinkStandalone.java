package com.verr1.controlcraft.foundation.cimulink.standalone;

import com.verr1.controlcraft.foundation.cimulink.game.circuit.CircuitNbt;
import com.verr1.controlcraft.foundation.cimulink.standalone.projects.DigitalCircuits;
import com.verr1.controlcraft.foundation.cimulink.standalone.projects.FPV;
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
        save(DigitalCircuits.cycleAdder(), Path.of(DataPath), "cycleAdder");
        save(DigitalCircuits.integralUnit(), Path.of(DataPath), "integralUnit");
    }

}

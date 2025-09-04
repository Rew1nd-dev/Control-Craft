package com.verr1.controlcraft.unstable.data.schematic;

import com.verr1.controlcraft.utils.CompoundTagBuilder;
import net.minecraft.nbt.CompoundTag;
import net.minecraftforge.fml.loading.FMLPaths;

import java.nio.file.Path;
import java.util.Objects;

public class SchematicKey {
    public static final SchematicKey EMPTY = new SchematicKey("empty", "empty");

    final String namespace;
    final String name;

    public SchematicKey(String namespace, String name) {
        this.namespace = namespace;
        this.name = name;
    }

    public String getFullName(){
        return namespace + "-" + name;
    }

    public CompoundTag serialize(){
        return CompoundTagBuilder.create()
                .withString("namespace", namespace)
                .withString("name", name)
                .build();
    }

    public static SchematicKey deserialize(CompoundTag tag) {
        String namespace = tag.getString("namespace");
        String name = tag.getString("name");
        return new SchematicKey(namespace, name);
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (!(object instanceof SchematicKey that)) return false;
        return Objects.equals(namespace, that.namespace) && Objects.equals(name, that.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(namespace, name);
    }
}

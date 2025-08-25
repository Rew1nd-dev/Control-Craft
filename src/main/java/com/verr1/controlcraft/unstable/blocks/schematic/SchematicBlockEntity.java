package com.verr1.controlcraft.unstable.blocks.schematic;

import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.AIServer;
import com.verr1.controlcraft.unstable.blocks.AIBaseBlockEntity;
import com.verr1.controlcraft.unstable.management.AIPool;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.valkyrienskies.core.api.ships.ServerShip;

public class SchematicBlockEntity extends AIBaseBlockEntity {

    public static final NetworkKey EXPORT_SCHEMATIC = NetworkKey.create("export_schematic");
    public static final NetworkKey NAMESPACE = NetworkKey.create("namespace");
    public static final NetworkKey NAME = NetworkKey.create("name");



    private String namespace = "default";
    private String name = AIPool.randomSequence(5);

    public SchematicBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);

        buildRegistry(NAMESPACE)
                .withBasic(SerializePort.of(this::namespace, this::setNamespace, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        buildRegistry(NAME)
                .withBasic(SerializePort.of(this::name, this::setName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        panel().registerUnit(EXPORT_SCHEMATIC, this::createSchematic);


    }


    public String name() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String namespace() {
        return namespace;
    }

    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    private void createSchematic(){
        if(!isOnShip())return;
        ServerShip ship = getLoadedServerShip();
        if(ship == null)return;
        AIServer.SCHEMATICS_MANAGER.createSchematicsAsync(getWorldBlockPos(), ship, namespace(), name());
    }

}

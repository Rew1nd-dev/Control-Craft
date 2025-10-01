package com.verr1.controlcraft.unstable.blocks.deploy;

import com.verr1.controlcraft.foundation.data.control.ImmutablePhysPose;
import com.verr1.controlcraft.foundation.data.control.ImmutableVel;
import com.verr1.controlcraft.foundation.network.executors.ClientBuffer;
import com.verr1.controlcraft.foundation.network.executors.SerializePort;
import com.verr1.controlcraft.unstable.ai.game.SharedAIKeys;
import com.verr1.controlcraft.unstable.data.schematic.SchematicKey;
import com.verr1.controlcraft.unstable.valkyrienskies.attachments.AIBlockNetwork;
import com.verr1.controlcraft.utils.SerializeUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;
import org.valkyrienskies.core.api.ships.ServerShip;

import static com.verr1.controlcraft.unstable.blocks.schematic.SchematicBlockEntity.*;

public class DirectDeployerBlockEntity extends AbstractDeployerBlockEntity {

    private String namespace = "";
    private String name = "";
    private boolean transformWithLocal = false;

    public boolean inherit() {
        return inheritVelOmg;
    }

    public void setInherit(boolean inheritVelOmg) {
        this.inheritVelOmg = inheritVelOmg;
    }

    private boolean inheritVelOmg = false;


    private Vector3d offset = new Vector3d();
    private Vector3d ypr = new Vector3d();

    private boolean deployNextTick = false;

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

    public DirectDeployerBlockEntity(BlockEntityType<?> type, BlockPos pos, BlockState state) {
        super(type, pos, state);
        buildRegistry(NAMESPACE)
                .withBasic(SerializePort.of(this::namespace, this::setNamespace, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        buildRegistry(NAME)
                .withBasic(SerializePort.of(this::name, this::setName, SerializeUtils.STRING))
                .withClient(ClientBuffer.STRING.get())
                .register();

        buildRegistry(SharedAIKeys.ROT)
                .withBasic(SerializePort.of(this::ypr, this::setYpr, SerializeUtils.VECTOR3D))
                .withClient(ClientBuffer.VECTOR3D.get())
                .register();

        buildRegistry(SharedAIKeys.POS)
                .withBasic(SerializePort.of(this::offset, this::setOffset, SerializeUtils.VECTOR3D))
                .withClient(ClientBuffer.VECTOR3D.get())
                .register();

        buildRegistry(SharedAIKeys.LOC)
                .withBasic(SerializePort.of(this::transformWithLocal, this::setTransformWithLocal, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        buildRegistry(SharedAIKeys.IHR)
                .withBasic(SerializePort.of(this::inherit, this::setInherit, SerializeUtils.BOOLEAN))
                .withClient(ClientBuffer.BOOLEAN.get())
                .register();

        panel().registerUnit(SharedAIKeys.ACTION, this::deploy);

    }

    public void deploy(){
        deployNextTick = true;
    }

    public Vector3d offset() {
        return offset;
    }

    public void setOffset(Vector3d offset) {
        this.offset = new Vector3d(offset);
    }

    public Vector3d ypr() {
        return ypr;
    }

    public void setYpr(Vector3d ypr) {
        this.ypr = new Vector3d(ypr);
    }

    public boolean transformWithLocal() {
        return transformWithLocal;
    }

    public void setTransformWithLocal(boolean transformWithLocal) {
        this.transformWithLocal = transformWithLocal;
    }

    @Override
    protected SchematicKey nextType() {
        return new SchematicKey(namespace, name);
    }

    @Override
    protected void onTickEnd() {
        deployNextTick = false;
    }

    @Override
    protected ImmutablePhysPose nextPose() {
        Vector3dc offset_wc = transformWithLocal() ? readSelf().s2wTransform().transformDirection(offset, new Vector3d()) : offset;
        Quaterniondc rot = transformWithLocal() ? new Quaterniond(readSelf().quaternion()) : new Quaterniond();
        return new ImmutablePhysPose(getBasePosition().add(offset_wc), rot.mul(getOffsetRotation(), new Quaterniond()));
    }

    @Override
    protected ImmutableVel nextVel() {
        return inherit() ? new ImmutableVel(getBaseVelocity(), readSelf().omega()) : ImmutableVel.ZERO;
    }




    public Quaterniond getOffsetRotation(){
        return new Quaterniond().rotationYXZ(
                Math.toRadians(ypr.y),
                Math.toRadians(ypr.x),
                Math.toRadians(ypr.z)
        );
    }

    @Override
    protected boolean shouldDeploy() {
        return deployNextTick;
    }

    @Override
    protected void onDeploy(@NotNull ServerShip ship, @Nullable AIBlockNetwork network, @NotNull SchematicKey type) {
        deployNextTick = false;
    }

    @Override
    protected void onDiscard(@NotNull ServerShip ship, @Nullable AIBlockNetwork network) {

    }
}

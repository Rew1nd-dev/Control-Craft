package com.verr1.controlcraft.ponder.instructions;

import com.simibubi.create.foundation.ponder.PonderScene;
import com.simibubi.create.foundation.ponder.instruction.PonderInstruction;
import com.simibubi.create.foundation.ponder.instruction.TickingInstruction;
import com.verr1.controlcraft.content.links.CimulinkRenderer;
import com.verr1.controlcraft.ponder.elements.CimulinkPonderOutliner;
import com.verr1.controlcraft.ponder.render.CimulinkPonderWireEntry;
import com.verr1.controlcraft.utils.MinecraftUtils;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.phys.Vec3;
import org.joml.Vector3dc;

import static com.verr1.controlcraft.utils.MinecraftUtils.toVec3;
import static org.valkyrienskies.mod.api.ValkyrienSkies.toJOML;

public class CimulinkWireInstruction extends TickingInstruction {
    public static class Remove extends TickingInstruction{
        private final Object slot;
        public Remove(Object slot) {
            super(false, 1);
            this.slot = slot;
        }

        @Override
        protected void firstTick(PonderScene scene) {
            CimulinkPonderOutliner.getOrCreate().remove(slot);
        }
    }

    private BlockPos start = BlockPos.ZERO;
    private BlockPos end = BlockPos.ZERO.offset(1, 0, 0);
    private Direction startDir = Direction.UP;
    private Direction endDir = Direction.UP;

    private double startOffset = 0.4;
    private double endOffset = 0.4;

    private final Object slot;

    private int inIndex = 0;
    private int inSize = 1;
    private int outIndex = 0;
    private int outSize = 1;

    private boolean eternal = false;

    private CimulinkWireInstruction(Object slot) {
        super(false, 1);
        this.slot = slot;
        eternal();
    }

    public static CimulinkWireInstruction add(Object slot){
        return new CimulinkWireInstruction(slot);
    }

    public static CimulinkWireInstruction.Remove remove(Object slot){
        return new Remove(slot);
    }

    public CimulinkWireInstruction time(int ticks){
        remainingTicks = totalTicks = ticks;
        eternal = false;
        return this;
    }

    public CimulinkWireInstruction eternal(){
        time(1);
        eternal = true;
        return this;
    }

    public CimulinkWireInstruction fromTo(BlockPos start, BlockPos end){
        this.start = start;
        this.end = end;
        return this;
    }

    public CimulinkWireInstruction inIndex(int index, int size){
        this.inIndex = index;
        this.inSize = size;
        return this;
    }

    public CimulinkWireInstruction startOffset(double offset){
        this.startOffset = offset;
        return this;
    }

    public CimulinkWireInstruction endOffset(double offset){
        this.endOffset = offset;
        return this;
    }

    public CimulinkWireInstruction outIndex(int index, int size){
        this.outIndex = index;
        this.outSize = size;
        return this;
    }

    public CimulinkWireInstruction directions(Direction start, Direction end) {
        this.startDir = start;
        this.endDir = end;
        return this;
    }

    public CimulinkWireInstruction startDirection(Direction start) {
        this.startDir = start;
        return this;
    }

    public CimulinkWireInstruction endDirection(Direction end) {
        this.endDir = end;
        return this;
    }



    private static Direction getVertical(Direction dir){
        return MinecraftUtils.getVerticalDirectionSimple(dir);
    }

    private static Direction getHorizontal(Direction dir){
        return getVertical(dir).getClockWise(dir.getAxis());
    }

    @Override
    public void reset(PonderScene scene) {
        super.reset(scene);
        CimulinkPonderOutliner.getOrCreate().remove(slot);
    }

    public Vec3 computeInputPortOffset(){
        double x = -CimulinkRenderer.deltaX(inIndex, inSize);//-0.25;
        double y =  CimulinkRenderer.deltaY(inIndex, inSize);

        Vec3 h = toVec3(getHorizontal(startDir).getNormal());
        Vec3 v = toVec3(getVertical(startDir).getNormal());
        return h.scale(x).add(v.scale(y));
    }

    public Vec3 computeOutputPortOffset(){
        double x = CimulinkRenderer.deltaX(outIndex, outSize);//0.25;
        double y =  CimulinkRenderer.deltaY(outIndex, outSize);

        Vec3 h = toVec3(getHorizontal(endDir).getNormal());
        Vec3 v = toVec3(getVertical(endDir).getNormal());
        return h.scale(x).add(v.scale(y));
    }




    @Override
    public void tick(PonderScene scene) {
        super.tick(scene);
        CimulinkPonderOutliner.getOrCreate().showLine(slot, this::createWire).eternal(eternal);
    }

    private CimulinkPonderWireEntry createWire(){
        Vector3dc startVec = toJOML(toVec3(startDir.getNormal()));
        Vector3dc endVec = toJOML(toVec3(endDir.getNormal()));
        return new CimulinkPonderWireEntry(
                toJOML(start.getCenter().add(computeOutputPortOffset())).fma(-startOffset, startVec),
                toJOML(end.getCenter().add(computeInputPortOffset())).fma(-endOffset, endVec),
                toJOML(toVec3(startDir.getNormal())),
                toJOML(toVec3(endDir.getNormal()))
        );

    }

    public static PonderInstruction initInstruction(){
        return PonderInstruction.simple(scene -> scene.addElement(CimulinkPonderOutliner.getOrCreate().resetVisibility()));
    }

}

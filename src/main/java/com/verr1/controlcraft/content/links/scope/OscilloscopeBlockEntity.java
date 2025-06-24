package com.verr1.controlcraft.content.links.scope;

import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.content.links.CimulinkRenderer;
import com.verr1.controlcraft.foundation.cimulink.game.port.inout.MultiOutputLinkPort;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.links.ConnectionStatus;
import com.verr1.controlcraft.foundation.data.links.ValueStatus;
import net.minecraft.core.BlockPos;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;

import java.util.*;
import java.util.stream.Stream;

public class OscilloscopeBlockEntity extends CimulinkBlockEntity<MultiOutputLinkPort> {
    public static final int MAX_CHANNELS = 4;

    public final Channels clientReceivedData = new Channels();
    public static final NetworkKey ADD_CHANNEL = NetworkKey.create("add_channel");
    public static final NetworkKey REMOVE_CHANNEL = NetworkKey.create("remove_channel");

    private int mainChannel = 0;

    public OscilloscopeBlockEntity(BlockEntityType<?> typeIn, BlockPos pos, BlockState state) {
        super(typeIn, pos, state);

        panel().registerUnit(ADD_CHANNEL, this::addChannel);
        panel().registerUnit(REMOVE_CHANNEL, this::removeChannel);

        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> ((CimulinkRenderer)renderer()).setSocketRenderOffset(0.2));
    }

    public double peekMain(){
        return clientReceivedData.peek();
    }

    @Override
    protected MultiOutputLinkPort create() {
        return new MultiOutputLinkPort();
    }

    public void setSize(int size){
        if(size > MAX_CHANNELS || size <= 0)return;
        linkPort().setSize(size);
    }

    public int size(){
        return linkPort().size();
    }

    public void addChannel(){
        setSize(size() + 1);
    }

    public void removeChannel(){
        setSize(size() - 1);
    }

    @Override
    public void tickClient() {
        super.tickClient();
        clientReceivedData.tick();
    }



    public static class Plots{
        public final static int PLOT_SIZE = 32;
        Queue<Double> values = new ArrayDeque<>(PLOT_SIZE + 1);

        public void receive(double value){
            if(values.size() >= PLOT_SIZE) {
                values.poll();
            }
            values.offer(value);
        }

        public double peek(){
            return values.isEmpty() ? 0.0 : values.peek();
        }

        public void clear(){
            values.clear();
        }

        public List<Double> valuesCopy() {
            return List.copyOf(values);
        }

        public Stream<Double> stream(){
            return values.stream();
        }
    }

    public class Channels{
        public List<Plots> plots = new ArrayList<>();
        public boolean nothingChanged = false;

        public void tick(){
            nothingChanged = isValueStatusDirty();
            if(nothingChanged)return;
            setValueStatueDirty();
            ConnectionStatus cs = readClientConnectionStatus();
            ValueStatus vs = readClientValueStatus();
            if(cs == null || vs == null)return;
            int validSize = Math.min(vs.inputValues.size(), cs.inputs.size());
            setSize(validSize);
            for (int i = 0; i < validSize; i++){
                plots.get(i).receive(vs.inputValues.get(i));
            }

        }

        public double peek(){
            return !plots.isEmpty() ? plots.get(0).peek() : 0.0;
        }

        private void setSize(int n){
            if(plots.size() == n)return;
            if(plots.size() < n){
                for(int i = plots.size(); i < n; i++){
                    plots.add(new Plots());
                }
            } else {
                plots = plots.subList(0, n);
            }
        }
    }
}

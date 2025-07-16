package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.util.concurrent.ListenableFuture;
import com.google.common.util.concurrent.ListenableFutureTask;
import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.ControlCraftServer;
import com.verr1.controlcraft.content.links.CimulinkBlockEntity;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.components.general.Temporal;
import com.verr1.controlcraft.foundation.cimulink.core.components.sources.SignalGenerator;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.debug.Debug;
import com.verr1.controlcraft.foundation.cimulink.game.debug.TestEnvBlockLinkWorld;
import com.verr1.controlcraft.foundation.cimulink.game.exceptions.EncloseLoopException;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import com.verr1.controlcraft.utils.Serializer;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

// for connection recording, forming a graph
public abstract class BlockLinkPort {

    public static boolean RUN_AT_PHYSICS_THREAD = false;
    public static boolean DEBUG_STEPPING_MODE = false;

    public static final Set<WorldBlockPos> ALL_BLP = ConcurrentHashMap.newKeySet();


    private static final LoadingCache<WorldBlockPos, Optional<BlockLinkPort>> CACHE = CacheBuilder.newBuilder()
            .maximumSize(1024)
            .refreshAfterWrite(2, TimeUnit.SECONDS)
            .build(
                    new CacheLoader<>() {
                        @Override
                        public @NotNull ListenableFuture<Optional<BlockLinkPort>> reload(
                                @NotNull WorldBlockPos key,
                                @NotNull Optional<BlockLinkPort> oldValue
                        ) {
                            ListenableFutureTask<Optional<BlockLinkPort>> task = ListenableFutureTask.create(() -> load(key));
                            ControlCraftServer.getMainThreadExecutor().execute(task);
                            return task;
                        }

                        @Override
                        public @NotNull Optional<BlockLinkPort> load(@NotNull WorldBlockPos pos) {
                            return BlockEntityGetter.INSTANCE
                                    .getBlockEntityAt(
                                            pos.globalPos(),
                                            ILinkableBlock.class
                                    ).map(ILinkableBlock::linkPort);
                        }
                    });

    // make it concurrent

    public static final Serializer<HashMap<BlockPos, String>> POS_NAME_MAP =
            SerializeUtils.ofMap(SerializeUtils.BLOCK_POS, SerializeUtils.STRING);

    public static final Serializer<HashMap<String, BlockPort>> BACKWARD =
            SerializeUtils.ofMap(SerializeUtils.STRING, SerializeUtils.BLOCK_PORT);

    public static final Serializer<HashMap<String, Set<BlockPort>>> FORWARD =
            SerializeUtils.ofMap(SerializeUtils.STRING, SerializeUtils.ofSet(SerializeUtils.BLOCK_PORT));

    private static final Set<BlockPort> EMPTY = new HashSet<>();



    private final HashMap<String, Set<BlockPort>>   forwardLinks  = new HashMap<>();
    private final HashMap<String, BlockPort>        backwardLinks = new HashMap<>();

    private final Map<String, Set<BlockPort>>   forwardView  = Collections.unmodifiableMap(forwardLinks);
    private final Map<String, BlockPort>        backwardView = Collections.unmodifiableMap(backwardLinks);

    private WorldBlockPos portPos;

    private NamedComponent realTimeComponent;

    private boolean initialized = false;

    // private final BlockEntity owner;

    private static boolean onMainThread(){
        return Thread.currentThread() == ControlCraftServer.INSTANCE.getRunningThread();
    }

    public static Optional<CimulinkBlockEntity<?>> ofBlockEntity(WorldBlockPos pos){
        return BlockEntityGetter.get()
                .getBlockEntityAt(pos, CimulinkBlockEntity.class)
                .map(be -> (CimulinkBlockEntity<?>) be);
    }

    public static Optional<BlockLinkPort> of(WorldBlockPos pos){
        if(Debug.TEST_ENVIRONMENT){
            return ofDebug(pos);
        }
        if(onMainThread()){
            ofCache(pos); // refresh cache
            return ofActual(pos);
        }else {
            return ofCache(pos);
        }
    }

    private static Optional<BlockLinkPort> ofDebug(WorldBlockPos pos){
        return TestEnvBlockLinkWorld.get(pos);
    }

    private static Optional<BlockLinkPort> ofActual(WorldBlockPos pos){
        return BlockEntityGetter.get()
                .getBlockEntityAt(pos, ILinkableBlock.class)
                .map(ILinkableBlock::linkPort);
    }

    private static Optional<BlockLinkPort> ofCache(WorldBlockPos pos){
        try{
            return CACHE.get(pos);
        }catch (Exception e){
            ControlCraft.LOGGER.error("Error loading BlockLinkPort at {}: {}", pos, e.getMessage());
            return Optional.empty();
        }
    }

    /*
    protected BlockLinkPort(WorldBlockPos portPos, NamedComponent initial) {
        this.portPos = portPos;
        realTimeComponent = initial;
        add(portPos);
    }
    * */

    protected BlockLinkPort(NamedComponent initial) {
        realTimeComponent = initial;
    }

    public void setWorldBlockPos(WorldBlockPos portPos){
        if(this.portPos == null){
            this.portPos = portPos;
            add(portPos);
        }else {
            throw new IllegalStateException("BlockLinkPort Pos has already been set!");
        }
    }

    public String name(){
        return realTimeComponent.name();
    }

    public void setName(String name){
        try{
            realTimeComponent.withName(name);
        }catch (IllegalArgumentException ignored){

        }
    }

    public static void propagateOutput(PropagateContext watcher, BlockLinkPort blp){
        blp.changedOutput().forEach(changedOutput -> {
            double value = blp.retrieveOutput(changedOutput);
            String changedOutputName = blp.outputsNames().get(changedOutput);

            blp.forwardLinks().getOrDefault(changedOutputName, EMPTY).stream().collect(Collectors.groupingBy(
                    BlockPort::pos, // group by BlockPos
                    HashMap::new,   // contained by HashMap
                    Collectors.mapping(
                            BlockPort::portName, // get String
                            Collectors.toList()  // collect to Set<String>
                    )
            )).forEach((worldBlockPos, portNames) -> {
                // filter out uninitialized blps
                of(worldBlockPos)
                .filter(BlockLinkPort::isInitialized)
                .ifPresent(nextBlp -> {
                    try{
                        portNames.forEach(n -> nextBlp.input(n, value));

                        // found output ports that need to propagate, should visit this current port
                        // After temporal update their output, they got changedOutput() not empty() already
                        // A temporal A0 may propagate its output to another temporal A1's input, although it won't cause
                        // immediate output change, but A1 still get non-empty changedOutput(), so propagation
                        // won't stop, which may cause watcher to mis judge a loop
                        // so stops whenever the nextBlp is temporal
                        if(nextBlp.isCombinational()) {
                            propagateCombinational(watcher.visit(blp.pos()), nextBlp);
                        }

                    }catch (IllegalArgumentException e){
                        ControlCraft.LOGGER.error("Error during propagation when trying propagate to:{}, exception: {}", nextBlp, e);
                    }catch (EncloseLoopException e){
                        ControlCraft.LOGGER.error("Enclosed loop detected: ", e);
                        BlockEntityGetter.playerAround(blp.pos(), 5).forEach(s ->
                                s.sendSystemMessage(Component.literal("Enclosed Loop Detected, A loop must contain at least one temporal circuit (shifter, ff etc)")));
                        blp.removeAllLinks();
                    }

                });
            });
        });
    }

    @Override
    public String toString() {
        return "[" + pos().pos().toShortString() + "| " + name() + "]";
    }

    public static void propagateInput(BlockLinkPort blp){
        blp.onInputChange(blp.changedInputName().toArray(new String[0]));
    }

    public static void propagateCombinational(PropagateContext watcher, BlockLinkPort blp){
        // don't propagate if the blp is not initialized
        if(!blp.isInitialized())return;
        // input changed, Component transit itself and update to output
        propagateInput(blp);
        // for all changed outputs, propagate to linked blp
        propagateOutput(watcher, blp);
    }

    // Splitting propagateGlobalInput() and propagateTemporal at PreTick() and PostTick() respectively, because
    // in a positive edge detector circuit, the changes of some combinational output lasts only between stage 0 - stage 2
    // Such change will be handled correctly when temporal update their input at stage 2, after which such output may change back
    // to what it is before stage 0
    // In this case, players won't see the visual feedback because between game ticks, the port value looks like remaining the same

    public static void propagateGlobalInput(){
        // stage 0
        // propagate changes caused by input link ports
        // after each propagateCombinational() done in forEach(), all components except input link port should
        // have their output up-to-date
        // so only input link ports are actually propagated, which means this will be equivalent to
        // ALL_BLP.forEach(wbp -> of(wbp).filter(BlockLinkPort::isSignal).ifPresent(blp -> blp.propagateInput()));
        ALL_BLP
        .forEach(wbp -> of(wbp)
        // filter out uninitialized ports
        .filter(BlockLinkPort::isInitialized)
        .ifPresent(
                blp -> propagateCombinational(new PropagateContext(), blp)
        ));
    }

    public static void propagateTemporal(){

        // stage 2
        // Temporal samples their input and update output, while combinational stay unchanged
        ALL_BLP.stream().map(BlockLinkPort::of).forEach(blp -> blp.ifPresent(BlockLinkPort::onPositiveEdge));

        // stage 3
        // Temporal output can be considered as a kind of input in a loop-less directional graph
        // Propagate changes caused by temporal outputs
        ALL_BLP
            .forEach(wbp -> of(wbp)
            .filter(BlockLinkPort::isInitialized)
            //.filter(BlockLinkPort::isNotSignal)
            .ifPresent(
                blp -> propagateCombinational(new PropagateContext(), blp)
        ));
    }

    public boolean anyOutputChanged(){
        return realTimeComponent.anyOutputChanged();
    }

    public boolean anyInputChanged(){
        return realTimeComponent.anyInputChanged();
    }

    public static void remove(WorldBlockPos pos){
        // ControlCraft.LOGGER.info("remove is called at: {}", pos);
        ALL_BLP.remove(pos);
    }

    public static void add(WorldBlockPos pos){
        // ControlCraft.LOGGER.info("add is called at: {}", pos);
        ALL_BLP.add(pos);
    }

    public static void validate(){
        List<WorldBlockPos> toRemove = ALL_BLP.stream().filter(wbp -> of(wbp).isEmpty()).toList();
        toRemove.forEach(BlockLinkPort::remove);
        ALL_BLP.forEach(wbp -> of(wbp).ifPresent(BlockLinkPort::removeInvalid));
    }

    public boolean isCombinational(){
        return !(__raw() instanceof Temporal<?>);
    }

    public boolean isNotSignal(){
        return !(__raw() instanceof SignalGenerator<?>);
    }

    public static void postMainTick(){
        if(RUN_AT_PHYSICS_THREAD)return;
        if(DEBUG_STEPPING_MODE)return;
        propagateTemporal();
    }

    public static void preMainTick(){
        if(RUN_AT_PHYSICS_THREAD)return;
        if(DEBUG_STEPPING_MODE)return;
        propagateGlobalInput();
    }


    public static void prePhysicsTick(){
        if(!RUN_AT_PHYSICS_THREAD)return;
        try{
            propagateGlobalInput();
            propagateTemporal();
        }catch (Exception e){
            ControlCraft.LOGGER.error("Error during physics tick propagation: {}", e.getMessage());
        }
    }

    public boolean isInitialized(){
        return initialized;
    }

    public void setInitialized(){
        initialized = true;
    }

    public int n(){
        return realTimeComponent.n();
    }

    public int m(){
        return realTimeComponent.m();
    }

    public void reset(){realTimeComponent.reset();}

    public Map<String, Set<BlockPort>> forwardLinks() {
        return forwardView;
    }

    public Map<String, BlockPort> backwardLinks() {
        return backwardView;
    }

    public final BlockPort __in(int index){
        return new BlockPort(pos(), realTimeComponent.in(index));
    }

    public final BlockPort __out(int index){
        return new BlockPort(pos(), realTimeComponent.out(index));
    }

    public final String in(int index){
        return realTimeComponent.in(index);
    }

    public final int in(String name){
        return realTimeComponent.in(name);
    }

    public final String out(int index){
        return realTimeComponent.out(index);
    }

    public final List<String> inputsNames(){
        return realTimeComponent.inputs();
    }



    public final List<String> inputsNamesExcludeSignals(){
        return realTimeComponent.inputsExcludeSignals(); // ().stream().filter(s -> !s.contains("@")).toList();
    }

    public final List<String> outputsNames(){
        return realTimeComponent.outputs();
    }

    public final List<Double> inputs(){
        return realTimeComponent.peekInput();
    }

    public final List<Double> outputs(){
        return realTimeComponent.peekOutput();
    }

    public final Map<String, Integer> nameOutputs() {
        return realTimeComponent.namedOutputs();
    }

    public final Map<String, Integer> nameInputs() {
        return realTimeComponent.namedInputs();
    }

    // should be called by top most input blp

    public void onPositiveEdge(){
        // ControlCraft.LOGGER.info("onPositiveEdge called at" + pos());
        realTimeComponent.onPositiveEdge();
    }

    public List<Integer> changedOutput(){
        return realTimeComponent.changedOutput();
    }

    public List<String> changedOutputName(){
        return realTimeComponent.changedOutputName();
    }

    protected List<Integer> changedInput(){
        return realTimeComponent.changedInput();
    }

    public List<String> changedInputName(){
        return realTimeComponent.changedInputName();
    }



    public void input(String inputPortName, double value){
        realTimeComponent.input(inputPortName, value);
    }

    public double output(String outputPortName){
        return realTimeComponent.output(outputPortName);
    }

    public void onInputDisconnection(String inputPortName){
        try{
            input(inputPortName, 0);
        }catch (Exception ignored){}
    }

    public void onOutputDisconnection(String outputPortName){}

    protected void deleteInput(String name){
        ControlCraft.LOGGER.info("deleting input: {} at: {}", name, pos());
        backwardLinks.remove(name);
        onInputDisconnection(name);
    }

    public void disconnectInput(String inputPortName){
        if(!backwardLinks.containsKey(inputPortName))return;
        BlockPort linked = backwardLinks.get(inputPortName);
        of(linked.pos()).ifPresent(p -> p.deleteOutput(inputPortName, new BlockPort(pos(), inputPortName)));

        deleteInput(inputPortName);

    }

    public void disconnectOutput(String outputPortName){
        forwardLinks.getOrDefault(outputPortName, EMPTY).forEach(blockPort -> {
            of(blockPort.pos()).ifPresent(p -> p.deleteInput(blockPort.portName()));
        });
        deleteOutput(outputPortName);

    }

    public void disconnectOutput(String outputPortName, BlockPort forwardPort){
        if(!forwardLinks.getOrDefault(outputPortName, EMPTY).contains(forwardPort))return;

        of(forwardPort.pos()).ifPresent(p -> p.deleteInput(forwardPort.portName()));

        deleteOutput(outputPortName);
    }

    protected void deleteOutput(String name, BlockPort forwardPort){
        ControlCraft.LOGGER.info("deleting output: {} -> {} at: {}", name, forwardPort, pos());
        forwardLinks.getOrDefault(name, EMPTY).remove(forwardPort);
        if(forwardLinks.getOrDefault(name, EMPTY).isEmpty())forwardLinks.remove(name);
        onOutputDisconnection(name);
    }

    public void deleteOutput(String name){
        ControlCraft.LOGGER.info("deleting all output: {} at: {}", name, pos());
        forwardLinks.remove(name);
        onOutputDisconnection(name);
    }

    public void removeAllLinks(){
        inputsNames().forEach(this::disconnectInput);
        outputsNames().forEach(this::disconnectOutput);
    }

    public void removeInvalidOutputKey(){
        forwardLinks.keySet().stream().filter(k -> !outputsNames().contains(k)).toList()
                .forEach(this::deleteOutput);
    }

    public void removeInvalidOutput(){
        removeInvalidOutputKey();
        forwardLinks.entrySet().stream().flatMap(e ->
            e.getValue().stream().map(bp -> new Pair<>(e.getKey(), bp))
        ).filter(e -> {

            if(!ready(e.getSecond().pos())){
                ControlCraft.LOGGER.info("block at: {} is not loaded fully, output: {} -> {}, skipping output check", pos(), e.getFirst(), e.getSecond());
                return false; // if the block is loaded, it is valid
            }

            String outputName = e.getFirst();
            BlockPort bp = e.getSecond();
            BlockLinkPort blp = of(bp.pos()).orElse(null);

            if(blp == null){
                ControlCraft.LOGGER.info("found null blp at: {} output: {} bp: {}", pos(), outputName, bp);
                return true;
            }

            /*
            * if(!ofBlockEntity(bp.pos()).map(CimulinkBlockEntity::initialized)
                    .orElseThrow(() -> new RuntimeException("How Can This Be be null? at: " + bp.pos()))
            ){
                ControlCraft.LOGGER.info("be at: {} haven't been initialized when checking output: {} ->: {}", bp.pos(), outputName, bp);
                return false;
            }
            * */

            boolean test = !blp.backwardLinks()
                    .getOrDefault(bp.portName(), BlockPort.EMPTY)
                    .equals(new BlockPort(pos(), outputName));

            // blp input port does not include this output port
            if(test){
                ControlCraft.LOGGER.info("output: {} -> {} is invalid, blp links: {}", new BlockPort(pos(), outputName), bp, blp.backwardLinks());
                return true;
            }

            return false;
        })
        .toList()
        .forEach(e -> {
            ControlCraft.LOGGER.info("call delete output from validation: {}", e);
            deleteOutput(e.getFirst(), e.getSecond());
        });

    }


    public void removeInvalidInputKey(){
        backwardLinks.keySet().stream().filter(k -> !inputsNames().contains(k)).toList()
                .forEach(this::deleteInput);
    }

    public void removeInvalidInput(){
        // List<String> invalidInputs = new ArrayList<>();
        removeInvalidInputKey();
        backwardLinks.entrySet().stream().filter(e -> {

            if(!ready(e.getValue().pos())){
                ControlCraft.LOGGER.info("block at: {} is not loaded fully, skipping input check", e.getValue().pos());
                return false;
            }

            String inputName = e.getKey();
            BlockPort bp = e.getValue();
            BlockLinkPort blp = of(bp.pos()).orElse(null);

            if(blp == null){
                ControlCraft.LOGGER.info("found null blp at: {} input: {} bp: {} when checking {}", bp.pos(), inputName, bp, pos());
                return true;
            }

            /*
            if(!ofBlockEntity(bp.pos())
                    .map(CimulinkBlockEntity::initialized)
                    .orElseThrow(() -> new RuntimeException("How Can This Be be null? at: " + bp.pos()))
            ){
                ControlCraft.LOGGER.info("be at: {} haven't been initialized when checking input: {} <-: {}", bp.pos(), inputName, bp);
                return false;
            }
            * */

            boolean test = !blp.forwardLinks()
                    .getOrDefault(bp.portName(), EMPTY)
                    .contains(new BlockPort(pos(), inputName));
            // blp output port does not include this input port
            if(test){
                ControlCraft.LOGGER.info("input: {} -> {} is invalid, blp links: {}", bp, new BlockPort(pos(), inputName), blp.forwardLinks());
                return true;
            }
            return false; // if the input is not in the blp, it is invalid
        })
        .toList()
        .forEach(e -> {
            ControlCraft.LOGGER.info("call delete input from validation: {}", e.getKey());
            deleteInput(e.getKey());
        });
    }

    public static boolean ready(WorldBlockPos wbp){
        if(!BlockEntityGetter.INSTANCE.isLoaded(wbp)){
            ControlCraft.LOGGER.info("ready() call --> state: unloaded at: {}", wbp);
            return false;
        }
        Optional<CimulinkBlockEntity<?>> oc = ofBlockEntity(wbp);
        if(oc.isPresent()){
            boolean initialized = oc.get().initialized();
            if(!initialized){
                ControlCraft.LOGGER.info("ready() call --> state: uninitialized at: {}", wbp);
            }
            return initialized;
        }
        return true; // continue check then we will see blp == null, which means this port is invalid
    }

    public @NotNull WorldBlockPos pos(){
        if(portPos == null){
            ControlCraft.LOGGER.warn("calling pos() before pos is set!");
            return WorldBlockPos.NULL;
        }
        return portPos;
    }

    public void quit(){
        remove(pos());
        removeAllLinks();
    }



    public void removeInvalid(){
        if(!isInitialized())return;
        removeInvalidInput();
        removeInvalidOutput();
    }

    public NamedComponent __raw(){
        return realTimeComponent;
    }

    public void connectTo(String outputPort, WorldBlockPos pos, String inputName) throws IllegalArgumentException{
        ArrayUtils.AssertPresence(outputsNames(), outputPort);

        BlockLinkPort blp = of(pos).orElse(null);
        if(blp == null)return;

        blp.connectBy(pos(), outputPort, inputName);
        forwardLinks.computeIfAbsent(outputPort, $ -> new HashSet<>()).add(new BlockPort(pos, inputName));
    }

    public final void connectBy(WorldBlockPos pos, String outputPort, String inputName) throws IllegalArgumentException{
        ArrayUtils.AssertPresence(inputsNames(), inputName); // should be a valid inputName
        ArrayUtils.AssertAbsence(backwardLinks.keySet(), inputName); // should not been connected
        backwardLinks.put(inputName, new BlockPort(pos, outputPort));
    }




    public abstract NamedComponent create();

    public final void recreate(){
        realTimeComponent = create();
        ControlCraft.LOGGER.info("calling recreate() at: {}", pos());
        inputsNames().forEach(this::disconnectInput);
        outputsNames().forEach(this::disconnectOutput);
    }

    public void onInputChange(String... changedInput) {
        realTimeComponent.onInputChange(changedInput);
    }

    public double retrieveOutput(Integer changedOutput) {
        return realTimeComponent.retrieveOutput(changedOutput);
    }


    public CompoundTag serializeLinks(){
        return CompoundTagBuilder.create()
                .withCompound("forward", serializeForward())
                .withCompound("backward", serializeBackward())
                .withCompound("name", SerializeUtils.STRING.serialize(name()))
                .build();
    }

    public CompoundTag serialize(){
        return serializeLinks();
    }

    public void deserialize(CompoundTag tag){
        deserializeLinks(tag);
        setInitialized();
    }

    public CompoundTag serializeForward(){
        return FORWARD.serialize(forwardLinks);
    }

    public void modifyWithOffset(BlockPos offset){
        ControlCraft.LOGGER.info("modifying with offset: " + offset.toShortString());
        Map<String, BlockPort> backwardLinksNew = new HashMap<>();
        Map<String, Set<BlockPort>> forwardLinksNew = new HashMap<>();
        backwardLinks.forEach((k, v) -> backwardLinksNew.put(k, v.offset(offset)));
        forwardLinks.forEach((k, vs) -> forwardLinksNew.put(k, vs.stream().map(v -> v.offset(offset)).collect(Collectors.toSet())));
        backwardLinks.clear();
        forwardLinks.clear();
        backwardLinks.putAll(backwardLinksNew);
        forwardLinks.putAll(forwardLinksNew);
    }

    public static Map<String, Set<BlockPort>> deserializeForward(CompoundTag tag){
        return FORWARD.deserialize(tag);
    }

    public static Map<String, BlockPort> deserializeBackward(CompoundTag tag){
        return BACKWARD.deserialize(tag);
    }

    public CompoundTag serializeBackward(){
        return BACKWARD.serialize(backwardLinks);
    }

    public void deserializeLinks(CompoundTag tag){
        forwardLinks.clear();
        backwardLinks.clear();
        forwardLinks.putAll(deserializeForward(tag.getCompound("forward")));
        backwardLinks.putAll(deserializeBackward(tag.getCompound("backward")));
        setName(SerializeUtils.STRING.deserializeOrElse(
                tag.getCompound("name"),
                Optional.of(realTimeComponent.getClass().getSimpleName()).filter(s -> !s.isEmpty())
                        .orElse(realTimeComponent.getClass().getSuperclass().getSimpleName())
        ));
    }

    public static class PropagateContext{
        private final int MAX_DEPTH = 128;
        private final ArrayList<WorldBlockPos> visited = new ArrayList<>();
        public int depth = 0;

        public PropagateContext(List<WorldBlockPos> visited, WorldBlockPos newPos) {
            this.visited.addAll(visited);
            this.visited.add(newPos);
        }


        public PropagateContext() {
        }

        public PropagateContext visit(WorldBlockPos pos){
            EncloseLoopDetection(pos);
            AssertValidPos(pos);
            return new PropagateContext(visited, pos);
        }

        public void AssertValidPos(WorldBlockPos pos){
            if(pos.equals(WorldBlockPos.NULL)){
                throw new IllegalArgumentException("Invalid WorldBlockPos When Propagating: " + pos);
            }
        }

        private String visitedMessage(){
            StringBuilder sb = new StringBuilder();
            visited.stream().map(BlockLinkPort::of).forEach(sb::append);
            return sb.toString();
        }

        public void EncloseLoopDetection(WorldBlockPos pos){
            try{
                ArrayUtils.AssertAbsence(visited, pos);
            }catch (Exception e){
                visited.add(pos);
                throw new EncloseLoopException("Enclosed Loop Detected: |" + visitedMessage());
            }

        }

    }

    public static void onClose(){
        ALL_BLP.clear();
        CACHE.invalidateAll();
        RUN_AT_PHYSICS_THREAD = false;
    }

}

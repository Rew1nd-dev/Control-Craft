package com.verr1.controlcraft.foundation.cimulink.game.port;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.BlockEntityGetter;
import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import com.verr1.controlcraft.foundation.cimulink.core.utils.ArrayUtils;
import com.verr1.controlcraft.foundation.cimulink.game.debug.Debug;
import com.verr1.controlcraft.foundation.cimulink.game.debug.TestEnvBlockLinkWorld;
import com.verr1.controlcraft.foundation.data.WorldBlockPos;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import com.verr1.controlcraft.utils.CompoundTagBuilder;
import com.verr1.controlcraft.utils.SerializeUtils;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;

import java.util.*;
import java.util.stream.Collectors;

// for connection recording, forming a graph
public abstract class BlockLinkPort {

    public static final Set<WorldBlockPos> ALL_BLP = new HashSet<>();

    public static final SerializeUtils.Serializer<HashMap<BlockPos, String>> POS_NAME_MAP =
            SerializeUtils.ofMap(SerializeUtils.BLOCK_POS, SerializeUtils.STRING);

    public static final SerializeUtils.Serializer<HashMap<String, BlockPort>> BACKWARD =
            SerializeUtils.ofMap(SerializeUtils.STRING, SerializeUtils.BLOCK_PORT);

    public static final SerializeUtils.Serializer<HashMap<String, Set<BlockPort>>> FORWARD =
            SerializeUtils.ofMap(SerializeUtils.STRING, SerializeUtils.ofSet(SerializeUtils.BLOCK_PORT));

    private static final Set<BlockPort> EMPTY = new HashSet<>();



    private final HashMap<String, Set<BlockPort>>   forwardLinks  = new HashMap<>();
    private final HashMap<String, BlockPort>        backwardLinks = new HashMap<>();

    private final Map<String, Set<BlockPort>>   forwardView  = Collections.unmodifiableMap(forwardLinks);
    private final Map<String, BlockPort>        backwardView = Collections.unmodifiableMap(backwardLinks);

    private final WorldBlockPos portPos;

    private NamedComponent realTimeComponent;


    protected BlockLinkPort(WorldBlockPos portPos, NamedComponent initial) {
        this.portPos = portPos;
        realTimeComponent = initial;
        add(portPos);
        if(Debug.TEST_ENVIRONMENT){
            TestEnvBlockLinkWorld.add(this);
        }
    }

    public String name(){
        return realTimeComponent.name();
    }

    public void setName(String name){
        realTimeComponent.withName(name);
    }

    public static void propagateOutput(PropagateContext watcher, BlockLinkPort blp){
        if(blp.anyInputChanged())watcher.visit(blp.pos());
        blp.changedOutput().forEach(changedOutput -> {
            double value = blp.retrieveOutput(changedOutput);
            String changedOutputName = blp.outputsNames().get(changedOutput);

            blp.forwardLinks().getOrDefault(changedOutputName, EMPTY).stream().collect(Collectors.groupingBy(
                    BlockPort::pos, // 按 BlockPos 分组
                    HashMap::new,   // 使用 HashMap 作为容器
                    Collectors.mapping(
                            BlockPort::portName, // 提取 String 值
                            Collectors.toList() // 收集到 Set<String>
                    )
            )).forEach((worldBlockPos, portNames) -> {
                of(worldBlockPos).ifPresent(nextBlp -> {
                    try{
                        portNames.forEach(n -> nextBlp.input(n, value));

                        propagateCombinational(watcher, nextBlp);
                    }catch (IllegalArgumentException e){
                        System.out.println("Error during propagation: " + e);
                        blp.removeAllLinks();
                        // ControlCraft.LOGGER.error("Error during propagation: {}", e.toString());
                    }

                });
            });
        });
    }

    public static void propagateInput(BlockLinkPort blp){
        blp.onInputChange(blp.changedInputName().toArray(new String[0]));
    }

    public static void propagateCombinational(PropagateContext watcher, BlockLinkPort blp){
        // detecting possible loop

        // input changed, Component transit itself and update to output
        propagateInput(blp);
        // for all changed outputs, propagate to linked blp
        propagateOutput(watcher, blp);
    }

    public boolean anyOutputChanged(){
        return realTimeComponent.anyOutputChanged();
    }

    public boolean anyInputChanged(){
        return realTimeComponent.anyInputChanged();
    }

    public static void propagateTemporal(){
        ALL_BLP.stream().map(BlockLinkPort::of).forEach(blp -> blp.ifPresent(BlockLinkPort::onPositiveEdge));
        // Temporal output can be considered as a kind of input in a loop-less directional graph
        ALL_BLP.forEach(wbp -> of(wbp).ifPresent(
                blp -> propagateCombinational(new PropagateContext(), blp)
        ));
    }

    public static void remove(WorldBlockPos pos){
        ALL_BLP.remove(pos);
    }

    public static void add(WorldBlockPos pos){
        ALL_BLP.add(pos);
    }

    public static void validate(){
        List<WorldBlockPos> toRemove = ALL_BLP.stream().filter(wbp -> of(wbp).isEmpty()).toList();
        toRemove.forEach(BlockLinkPort::remove);
        ALL_BLP.forEach(wbp -> of(wbp).ifPresent(BlockLinkPort::removeInvalid));
    }


    public static void postTick(){
        propagateTemporal();
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

    public final String out(int index){
        return realTimeComponent.out(index);
    }

    public final List<String> inputsNames(){
        return realTimeComponent.inputs();
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
        realTimeComponent.onPositiveEdge();
        //propagateOutput(new PropagateContext());
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

    public void deleteInput(String name){
        ControlCraft.LOGGER.info("deleting invalid input: {}", name);
        backwardLinks.remove(name);
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

    public void deleteOutput(String name, BlockPort forwardPort){
        ControlCraft.LOGGER.info("deleting invalid output: {} -> {}", name, forwardPort);
        forwardLinks.getOrDefault(name, EMPTY).remove(forwardPort);
        if(forwardLinks.getOrDefault(name, EMPTY).isEmpty())forwardLinks.remove(name);
    }

    public void deleteOutput(String name){
        forwardLinks.remove(name);
    }

    public void removeInvalidOutput(){
        forwardLinks.entrySet().stream().flatMap(e ->
            e.getValue().stream().map(bp -> new Pair<>(e.getKey(), bp))
        ).filter(e -> {
            String outputName = e.getFirst();
            BlockPort bp = e.getSecond();
            BlockLinkPort blp = of(bp.pos()).orElse(null);
            if(blp == null)return true;
            return !blp.backwardLinks()
                    .getOrDefault(bp.portName(), BlockPort.EMPTY)
                    .equals(new BlockPort(pos(), outputName)); // if the output is not in the blp, it is invalid
        })
        .toList()
        .forEach(e -> deleteOutput(e.getFirst(), e.getSecond()));

    }

    public void removeAllLinks(){
        inputsNames().forEach(this::disconnectInput);
        outputsNames().forEach(this::disconnectOutput);
    }

    public void removeInvalidInput(){
        // List<String> invalidInputs = new ArrayList<>();
        backwardLinks.entrySet().stream().filter(e -> {
            String inputName = e.getKey();
            BlockPort bp = e.getValue();
            BlockLinkPort blp = of(bp.pos()).orElse(null);
            if(blp == null)return true;
            return !blp.forwardLinks()
                    .getOrDefault(bp.portName(), EMPTY)
                    .contains(new BlockPort(pos(), inputName)); // if the input is not in the blp, it is invalid
        })
                .toList().forEach(e -> deleteInput(e.getKey()));
    }

    public WorldBlockPos pos(){
        return portPos;
    }

    public void quit(){
        remove(pos());
        removeAllLinks();
    }



    public void removeInvalid(){
        removeInvalidInput();
        removeInvalidOutput();
    }

    protected NamedComponent __raw(){
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

    public static Optional<BlockLinkPort> of(WorldBlockPos pos){
        if(Debug.TEST_ENVIRONMENT){
            return TestEnvBlockLinkWorld.get(pos);
        }
        return BlockEntityGetter.get()
                .getBlockEntityAt(pos, ILinkableBlock.class)
                .map(ILinkableBlock::linkPort);
    }


    public abstract NamedComponent create();

    public final void recreate(){
        realTimeComponent = create();
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
                .build();
    }

    public CompoundTag serialize(){
        return serializeLinks();
    }

    public void deserialize(CompoundTag tag){
        deserializeLinks(tag);
    }

    public CompoundTag serializeForward(){
        return FORWARD.serialize(forwardLinks);
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
    }

    public static class PropagateContext{
        private final int MAX_DEPTH = 128;
        private final Set<WorldBlockPos> visited = new HashSet<>();
        public int depth = 0;

        public PropagateContext increased(){
            PropagateContext newContext = new PropagateContext();
            newContext.depth = depth + 1;
            return newContext;
        }

        public PropagateContext visit(WorldBlockPos pos){
            EncloseLoopDetection(pos);
            visited.add(pos);
            return this;
        }

        private String visitedMessage(){
            StringBuilder sb = new StringBuilder();
            visited.stream().map(WorldBlockPos::pos).forEach(p -> sb.append(p.toShortString()).append("|"));
            return sb.toString();
        }

        public void EncloseLoopDetection(WorldBlockPos pos){
            try{
                ArrayUtils.AssertAbsence(visited, pos);
            }catch (Exception e){
                throw new IllegalArgumentException("Enclosed Loop Detected: " + visitedMessage());
            }

        }

    }

}

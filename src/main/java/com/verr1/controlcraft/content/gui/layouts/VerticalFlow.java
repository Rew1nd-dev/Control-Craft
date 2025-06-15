package com.verr1.controlcraft.content.gui.layouts;

import com.verr1.controlcraft.content.gui.factory.GenericUIFactory;
import com.verr1.controlcraft.content.gui.layouts.api.SwitchableTab;
import com.verr1.controlcraft.content.gui.layouts.element.MultipleTypedUIPort;
import com.verr1.controlcraft.content.gui.layouts.element.general.TypedUIPort;
import com.verr1.controlcraft.content.gui.layouts.element.general.UnitUIPanel;
import com.verr1.controlcraft.foundation.api.delegate.INetworkHandle;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.executor.executables.ConditionExecutable;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.layouts.GridLayout;
import net.minecraft.client.gui.layouts.Layout;
import net.minecraft.client.gui.layouts.LayoutElement;
import net.minecraft.client.gui.navigation.ScreenRectangle;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Player;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static com.verr1.controlcraft.ControlCraftClient.CLIENT_EXECUTOR;


public class VerticalFlow implements SwitchableTab {
    private final GridLayout verticalLayout = new GridLayout();
    private final List<NetworkKey> map;
    private final List<? extends NetworkUIPort<?>> entries; // this is meant to preserve the input order
    private final BlockPos boundBlockEntityPos;

    private final Runnable preDoLayout;
    private Component title;

    VerticalFlow(
            BlockPos boundPos,
            List<NetworkKey> keys,
            List<? extends NetworkUIPort<?>> entries,
            Runnable preDoLayout
    ){
        this.boundBlockEntityPos = boundPos;
        this.entries = entries;
        this.preDoLayout = preDoLayout;
        this.map = keys;
    }

    public static int debug_init_id = 0;

    public void onScreenInit(){
        entries.forEach(NetworkUIPort::onScreenInit);
        syncUI();
    }

    public void syncUI(){
        Minecraft mc = Minecraft.getInstance();
        Player p = mc.player;
        if(p == null)return;
        boundBlockEntity().ifPresentOrElse(
                be -> {
                    NetworkKey[] keys = map.toArray(NetworkKey[]::new);
                    be.handler().request(keys);
                    be.handler().setDirty(keys);
                    AtomicInteger row = new AtomicInteger();
                    entries.forEach(port -> verticalLayout.addChild(port.layout(), row.getAndIncrement(), 0, 1, 2));
                    verticalLayout.rowSpacing(4);
                    verticalLayout.arrangeElements();
                    debug_init_id++;
                    var task = new ConditionExecutable
                            .builder(() -> entries.forEach(v -> {
                                        v.readToLayout();
                                        v.onMessage(Message.POST_READ);
                            }))
                            .withCondition(() -> !be.handler().isAnyDirty(keys))
                            .withExpirationTicks(40)
                            .withOrElse(
                                    () -> p.sendSystemMessage(Component.literal("Block Entity Data Failed To Synced " + debug_init_id))
                            )
                            .build();
                    CLIENT_EXECUTOR.execute(task);
                },
                () -> p.sendSystemMessage(Component.literal("Block Entity Not Found !!!"))
        );
    }

    @Override
    public void onActivatedTab(){
        entries.forEach(NetworkUIPort::onActivatedTab);
    }

    @Override
    public void onRemovedTab() {
        entries.forEach(NetworkUIPort::onRemovedTab);
    }

    @Override
    public void onScreenTick() {
        entries.forEach(NetworkUIPort::onScreenTick);
    }

    @Override
    public void onAddRenderable(Collection<AbstractWidget> toAdd) {
        entries.forEach(p -> p.onAddRenderable(toAdd));
    }


    private void traverse(LayoutElement root, BiConsumer<WidgetContext, LayoutElement> consumer, Context context){
        consumer.accept(new WidgetContext(context.layer, root.getClass()), root);
        if (root instanceof Layout l) {
            l.visitChildren(c -> traverse(c, consumer, context.addLayer()));
        }
    }

    public void  traverseAllChildWidget(BiConsumer<WidgetContext, LayoutElement> consumer){
        traverse(verticalLayout, consumer, new Context());
    }

    public void onMessage(Message msg){
        entries.forEach(p -> p.onMessage(msg));
    }

    public void apply(){
        boundBlockEntity().ifPresent(
            be -> {
                entries.forEach(p -> p.onMessage(Message.PRE_APPLY));
                NetworkKey[] keys = map.toArray(NetworkKey[]::new);
                // be.activateLock(keys); // maybe not needed
                entries.forEach(NetworkUIPort::writeFromLayout);
                be.handler().syncToServer(keys);
                entries.forEach(p -> p.onMessage(Message.POST_APPLY));
            }
        );
    }


    private Optional<INetworkHandle> boundBlockEntity(){
        return GenericUIFactory.boundBlockEntity(boundBlockEntityPos, INetworkHandle.class);
    }

    @Override
    public @NotNull Component getTabTitle() {
        return this.title;
    }

    public VerticalFlow withTitle(Component title) {
        this.title = title;
        return this;
    }

    @Override
    public void visitChildren(@NotNull Consumer<AbstractWidget> consumer) {
        traverseAllChildWidget((ctx, w) -> {
            if(w instanceof AbstractWidget aw)consumer.accept(aw);
        });
    }

    @Override
    public void doLayout(@NotNull ScreenRectangle screenRectangle) {
        preDoLayout.run();
        this.verticalLayout.setX(screenRectangle.left() + 6);
        this.verticalLayout.setY(screenRectangle.top() + 6);
        this.verticalLayout.arrangeElements();
        // FrameLayout.alignInRectangle(this.verticalLayout, screenRectangle, 0.5F, 0.16666667F);
    }

    public static class Context{
        public int layer = 0;

        public Context addLayer(){
            layer++;
            return this;
        }

    }

    public record WidgetContext(int layer, Class<?> clazz){
    }

    public static class builder{
        BlockPos pos;
        List<NetworkUIPort<?>> ports = new ArrayList<>();
        List<NetworkKey> keys = new ArrayList<>();

        Runnable preDoLayout = () -> {};

        public builder(BlockPos pos){
            this.pos = pos;
        }

        public VerticalFlow build(){
            return new VerticalFlow(pos, keys, ports, preDoLayout);
        }

        public builder withPreDoLayout(Runnable postDoLayout){
            this.preDoLayout = postDoLayout;
            return this;
        }

        public builder withPort(
                NetworkKey key,
                NetworkUIPort<?> port
        ){
            this.keys.add(key);
            this.ports.add(port);
            return this;
        }

        public builder withPort(
                TypedUIPort<?>... port
        ){
            for(var p: port){
                withPort(p.key(), p);
            }
            return this;
        }

        public builder withPort(
                UnitUIPanel... port
        ){
            for(var p: port){
                this.keys.add(p.key());
                this.ports.add(p);
            }
            return this;
        }

        public builder withPort(MultipleTypedUIPort mt){
            keys.addAll(mt.keys());
            ports.add(mt);
            return this;
        }


    }


}

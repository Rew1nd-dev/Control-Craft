package com.verr1.controlcraft.content.gui.layouts;

import com.verr1.controlcraft.content.gui.layouts.api.TabListener;
import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.layouts.GridLayout;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.function.Consumer;
import java.util.function.Supplier;

public abstract class NetworkUIPort<T> implements TabListener {
//    private final Consumer<T> write;
//    private final Supplier<T> read;
    private final GridLayout layout = new GridLayout();
    protected boolean isActivated = false;

    public NetworkUIPort(){}

    protected abstract void consume(T data);

    protected abstract T provide();

    public void setParent(VerticalFlow parent){
        this.parent = parent;
    }

    public GridLayout layout(){
        return layout;
    }

    public VerticalFlow parent;

    public @Nullable VerticalFlow parent() {
        return parent;
    }

    public void onScreenInit(){
        initLayout(layout);
    }

    public void readToLayout(){
        writeGUI(provide());
    }

    public void onActivatedTab(){
        isActivated = true;
        layout.visitWidgets(e -> e.visible = true);
    }

    protected void redoLayout(){
        if(parent == null)return;
        parent.redoLayout();
    }

    @Override
    public void onAddRenderable(Collection<AbstractWidget> toAdd) {
        layout.visitWidgets(toAdd::add);
    }

    public void onRemovedTab(){
        isActivated = false;
        layout.visitWidgets(e -> e.visible = false);
    }


    public final void writeFromLayout(){
        T value = readGUI();
        if(value == null)return;
        consume(value);
    }


    public abstract void initLayout(GridLayout layoutToFill);
    public abstract T readGUI();
    public abstract void writeGUI(T value);


}

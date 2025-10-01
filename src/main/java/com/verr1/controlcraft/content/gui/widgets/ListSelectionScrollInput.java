package com.verr1.controlcraft.content.gui.widgets;

import com.simibubi.create.foundation.gui.element.ScreenElement;
import com.simibubi.create.foundation.gui.widget.ScrollInput;
import com.verr1.controlcraft.content.gui.layouts.api.SizedScreenElement;
import net.minecraft.network.chat.Component;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

public class ListSelectionScrollInput<T> extends IconSelectionScrollInput{

    public ListSelectionScrollInput(int xIn, int yIn, int widthIn, int heightIn, ScreenElement icon) {
        super(xIn, yIn, widthIn, heightIn, icon);
    }

    public ListSelectionScrollInput(SizedScreenElement icon) {
        super(icon);
    }

    private final List<T> options = new ArrayList<>();



    public ListSelectionScrollInput<T> forOptions(List<? extends T> options, Function<T, Component> toLabel){
        super.forOptions(options.stream().map(toLabel).toList());
        this.options.clear();
        this.options.addAll(options);
        return this;
    }

    public @NotNull T currentOption(){
        int state = getState();
        if(state < 0 || state >= options.size())throw new IndexOutOfBoundsException("State "+state+" is out of bounds for options of size "+options.size());
        return options.get(state);
    }

    public @NotNull Optional<T> currentOptionOpt(){
        int state = getState();
        if(state < 0 || state >= options.size())return Optional.empty();
        return Optional.of(options.get(state));
    }

}

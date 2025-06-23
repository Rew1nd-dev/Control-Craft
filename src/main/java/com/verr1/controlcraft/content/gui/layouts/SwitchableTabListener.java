package com.verr1.controlcraft.content.gui.layouts;

import com.verr1.controlcraft.content.gui.layouts.api.TabListener;
import net.minecraft.client.gui.components.AbstractWidget;

import java.util.Collection;

public interface SwitchableTabListener extends TabListener
{

    default void onDoLayout(){}

}

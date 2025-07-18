package com.verr1.controlcraft.content.gui.layouts.api;

import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.components.Renderable;
import net.minecraft.client.gui.components.events.GuiEventListener;
import net.minecraft.client.gui.narration.NarratableEntry;

import java.util.Collection;

public interface TabListener{

    default void onActivatedTab(){};

    default void onRemovedTab(){};

    default void onScreenTick(){};

    default void onMessage(Message msg){};

    default void onAddRenderable(Collection<AbstractWidget> toAdd){};

    default void onClose(){}

    record Message(String message){
        public static Message PRE_APPLY = new Message("pre_apply");
        public static Message POST_APPLY = new Message("post_apply");
        public static Message POST_DO_LAYOUT = new Message("post_do_layout");
        public static Message POST_READ = new Message("post_READ");

        @Override
        public boolean equals(Object o) {
            if(o instanceof Message m){
                return m.message.equals(message);
            }
            return false;
        }
    }
}

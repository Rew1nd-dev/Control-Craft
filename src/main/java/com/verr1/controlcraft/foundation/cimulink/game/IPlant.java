package com.verr1.controlcraft.foundation.cimulink.game;

import com.verr1.controlcraft.foundation.cimulink.core.components.NamedComponent;
import org.jetbrains.annotations.NotNull;

public interface IPlant {

    @NotNull NamedComponent plant();

    default String getName(){
        return plant().name();
    }

    default void setName(String name){
        plant().withName(name);
    }

}

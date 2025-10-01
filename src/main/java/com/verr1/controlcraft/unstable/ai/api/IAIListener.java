package com.verr1.controlcraft.unstable.ai.api;

public interface IAIListener {

    default void onSpawn(){};

    default void onDiscard(){};

    default void onPreRepair(){};

    default void onPostRepair(){};

    default void onPreRestore(){};

    default void onPostRestore(){};

    default void onProjectileImpact(){};

}

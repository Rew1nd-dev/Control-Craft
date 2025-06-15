package com.verr1.controlcraft;


import com.verr1.controlcraft.content.compact.tweak.impl.RedstoneLinkNetworkHandlerExtension;
import com.verr1.controlcraft.foundation.executor.Executor;
import com.verr1.controlcraft.foundation.managers.PeripheralNetwork;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraftforge.server.ServerLifecycleHooks;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.config.Configurator;

public class ControlCraftServer {
    public static MinecraftServer INSTANCE;
    public static ServerLevel OVERWORLD;
    public static RedstoneLinkNetworkHandlerExtension DECIMAL_LINK_NETWORK_HANDLER = new RedstoneLinkNetworkHandlerExtension();
    public static PeripheralNetwork CC_NETWORK = new PeripheralNetwork();
    public static final Executor SERVER_EXECUTOR = new Executor();

    public static void ServerInit(){
        Configurator.setLevel("org.valkyrienskies.core.impl.networking", Level.ERROR);
        Configurator.setLevel("org.valkyrienskies.core.networking", Level.ERROR);
        Configurator.setLevel("org.valkyrienskies.physics.networking", Level.ERROR);
    }

    public static java.util.concurrent.Executor getMainThreadExecutor() {
        return task -> {
            MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
            if (server != null) {
                server.execute(task);
            } else {
                // 如果服务器尚未初始化，抛出异常或直接运行（视需求而定）
                throw new IllegalStateException("MinecraftServer is not available. Ensure this is called after server initialization.");
            }
        };
    }

}

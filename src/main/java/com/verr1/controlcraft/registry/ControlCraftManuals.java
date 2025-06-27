package com.verr1.controlcraft.registry;

import com.verr1.controlcraft.ControlCraft;
import li.cil.manual.api.ManualModel;
import li.cil.manual.api.prefab.Manual;
import li.cil.manual.api.prefab.provider.NamespaceDocumentProvider;
import li.cil.manual.api.provider.DocumentProvider;
import li.cil.manual.api.util.Constants;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;

public class ControlCraftManuals {

    public static final DeferredRegister<ManualModel> MANUALS = DeferredRegister.create(Constants.MANUAL_REGISTRY, ControlCraft.MODID);
    public static final RegistryObject<ManualModel> MANUAL = MANUALS.register("controlcraft_common", Manual::new);

    public static final DeferredRegister<DocumentProvider> DOCUMENT_PROVIDERS = DeferredRegister.create(Constants.DOCUMENT_PROVIDER_REGISTRY, ControlCraft.MODID);
    public static final RegistryObject<DocumentProvider> DOCUMENT_PROVIDER = DOCUMENT_PROVIDERS.register("all_manuals", () ->
            new NamespaceDocumentProvider(ControlCraft.MODID, "doc"));




    public static void register(IEventBus modEventBus){
        MANUALS.register(modEventBus);
        DOCUMENT_PROVIDERS.register(modEventBus);
    }

}

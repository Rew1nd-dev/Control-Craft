package com.verr1.controlcraft.registry;

import com.simibubi.create.AllCreativeModeTabs;
import com.simibubi.create.foundation.data.CreateRegistrate;
import com.simibubi.create.foundation.utility.Components;
import com.tterrag.registrate.util.entry.RegistryEntry;
import com.verr1.controlcraft.ControlCraft;
import net.minecraft.core.registries.Registries;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;

import java.util.List;

public class ControlCraftCreativeTabs {
    private static final DeferredRegister<CreativeModeTab> REGISTER =
            DeferredRegister.create(Registries.CREATIVE_MODE_TAB, ControlCraft.MODID);

    public static final RegistryObject<CreativeModeTab> MAIN = REGISTER.register("main",
            () -> CreativeModeTab.builder()
                    .title(Components.translatable("itemGroup."+ ControlCraft.MODID +".main"))
                    .withTabsBefore(AllCreativeModeTabs.BASE_CREATIVE_TAB.getKey())
                    .icon(ControlCraftBlocks.CONSTRAINT_SERVO_MOTOR_BLOCK::asStack)
                    .displayItems((params, output) -> {

                        List<ItemStack> items = ControlCraft.REGISTRATE.getAll(Registries.ITEM)
                                .stream()
                                .filter(e -> CreateRegistrate.isInCreativeTab(e, ControlCraftCreativeTabs.MAIN))
                                .map(RegistryEntry::get)
                                .map(ItemStack::new)
                                .toList();

                        output.acceptAll(items);

                    })
                    .build());

    public static final RegistryObject<CreativeModeTab> CIMULINK = REGISTER.register("circuits",
            () -> CreativeModeTab.builder()
                    .title(Components.translatable("itemGroup."+ ControlCraft.MODID +".cimulink"))
                    .withTabsBefore(AllCreativeModeTabs.BASE_CREATIVE_TAB.getKey())
                    .icon(CimulinkBlocks.LOGIC_GATE::asStack)
                    .displayItems((params, output) -> {

                        List<ItemStack> items = ControlCraft.REGISTRATE.getAll(Registries.ITEM)
                                .stream()
                                .filter(e -> CreateRegistrate.isInCreativeTab(e, ControlCraftCreativeTabs.CIMULINK))
                                .map(RegistryEntry::get)
                                .map(ItemStack::new)
                                .toList();

                        output.acceptAll(items);

                    })
                    .build());

    /*
    *
    *
    * */

    public static void register(IEventBus modEventBus) {
        ControlCraft.LOGGER.info("Registering Creative Tabs");
        REGISTER.register(modEventBus);
    }
}

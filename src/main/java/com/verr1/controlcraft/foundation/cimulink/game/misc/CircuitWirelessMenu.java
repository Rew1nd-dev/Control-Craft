package com.verr1.controlcraft.foundation.cimulink.game.misc;

import com.simibubi.create.foundation.gui.menu.GhostItemMenu;
import com.verr1.controlcraft.content.gui.container.PageItemSlot;
import com.verr1.controlcraft.content.links.circuit.CircuitBlockEntity;
import com.verr1.controlcraft.content.links.circuit.WrappedChannel;
import com.verr1.controlcraft.foundation.redstone.TerminalMenu;
import com.verr1.controlcraft.registry.ControlCraftMenuTypes;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.MenuType;
import net.minecraftforge.items.ItemStackHandler;

import static com.verr1.controlcraft.content.gui.screens.TerminalScreen.LINE_BLOCK;

public class CircuitWirelessMenu extends GhostItemMenu<WrappedChannel> {


    public CircuitWirelessMenu(MenuType<?> type, int id, Inventory inv, FriendlyByteBuf extraData) {
        super(type, id, inv, extraData);
    }

    public CircuitWirelessMenu(MenuType<?> type, int id, Inventory inv, WrappedChannel contentHolder) {
        super(type, id, inv, contentHolder);
    }

    public static CircuitWirelessMenu create(int id, Inventory inv, WrappedChannel packet){
        return new CircuitWirelessMenu(ControlCraftMenuTypes.CIRCUIT.get(), id, inv, packet);
    }

    @Override
    protected ItemStackHandler createGhostInventory() {
        return CircuitBlockEntity.getFrequencyItems(contentHolder);
    }

    @Override
    protected boolean allowRepeats() {
        return true;
    }

    @Override
    protected WrappedChannel createOnClient(FriendlyByteBuf extraData) {
        return new WrappedChannel(extraData);
    }

    public void setPage(int page){
        slots.stream().filter(PageItemSlot.class::isInstance).map(PageItemSlot.class::cast).forEach(
                s -> s.setActive(s.page == page)
        );
    }

    public void setPage(int page, int maxLine){
        slots.stream().filter(PageItemSlot.class::isInstance).map(PageItemSlot.class::cast).forEach(
                s -> s.setActive(s.page == page && s.line < maxLine)
        );
    }

    @Override
    protected void addSlots() {
        addPlayerSlots(40 + 8, 131 + 24);

        int totalPages = (contentHolder.size() - 1) / LINE_BLOCK + 1;

        int x = 8;
        int y = 8;
        int slot = 0;

        for(int p = 0; p < totalPages; p++){
            for (int row = 0; row < LINE_BLOCK; ++row) {
                for (int column = 0; column < 2; ++column){
                    if(slot >= ghostInventory.getSlots())break;
                    addSlot(new PageItemSlot(ghostInventory, slot++, x + column * 18, y + row * 18)
                            .withPage(p)
                            .withLine(row)
                            .active(p == 0)
                    );
                }
            }

        }
    }

    @Override
    public void clicked(int slotId, int dragType, ClickType clickTypeIn, Player player) {
        if (slotId == playerInventory.selected && clickTypeIn != ClickType.THROW)
            return;
        super.clicked(slotId, dragType, clickTypeIn, player);
    }

    @Override
    public boolean stillValid(Player playerIn) {
        return true;
    }

    @Override
    protected void saveData(WrappedChannel contentHolder) {
        contentHolder.serialize(ghostInventory.serializeNBT());
    }
}

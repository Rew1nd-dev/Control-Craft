package com.verr1.controlcraft.content.links.computer;

import java.util.*;

public class ComputerNetworkHandler {

    private final Map<String, Object> slots = new HashMap<>();
    private final Set<String> dirtyFlags = new HashSet<>();

    public void set(String slot, Object value) {
        slots.put(slot, value);
        dirtyFlags.add(slot);
    }

    public boolean isDirty(String slot) {
        return dirtyFlags.contains(slot);
    }

    public boolean isAnyDirty() {
        return !dirtyFlags.isEmpty();
    }

    public Object peek(String slot) {
        return slots.get(slot);
    }

    public Object retrieve(String slot) {
        dirtyFlags.remove(slot);
        return peek(slot);
    }

    public Map<String, Object> allData(){
        return Collections.unmodifiableMap(slots);
    }

    public Map<String, Object> collectData() {
        if (dirtyFlags.isEmpty()) return null;

        Map<String, Object> delta = new HashMap<>();
        for (String slot : dirtyFlags) {
            delta.put(slot, slots.get(slot));
        }

        dirtyFlags.clear();
        return delta;
    }

}
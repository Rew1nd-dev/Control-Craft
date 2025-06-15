package com.verr1.controlcraft.content.gui.layouts.element.general;

import com.verr1.controlcraft.content.gui.layouts.api.LabelProvider;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import net.minecraft.core.BlockPos;

public class StringUIField extends BasicUIField<String>{

    public StringUIField(
            BlockPos boundPos,
            NetworkKey key,
            LabelProvider titleProv
    ) {
        super(
                boundPos,
                key,
                String.class,
                "",
                titleProv,
                s -> s,
                s -> s
        );
    }
}

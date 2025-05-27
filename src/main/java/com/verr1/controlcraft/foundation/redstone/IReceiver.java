package com.verr1.controlcraft.foundation.redstone;

import com.verr1.controlcraft.foundation.data.NetworkKey;

public interface IReceiver {

    NetworkKey FIELD = NetworkKey.create("field_");

    DirectReceiver receiver();

    String name();


}

package com.verr1.controlcraft.foundation.data.links;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class ConnectionStatus {

    public final Map<String, Set<BlockPort>> outputPorts = new HashMap<>();
    public final Map<String, BlockPort> inputPorts = new HashMap<>();


}

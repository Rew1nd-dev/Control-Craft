package com.verr1.controlcraft.foundation.cimulink.game.exceptions;

import org.luaj.vm2.LuaError;

public class UnpresentPortException extends LuaError {
    public UnpresentPortException(String message) {
        super(message);
    }
}

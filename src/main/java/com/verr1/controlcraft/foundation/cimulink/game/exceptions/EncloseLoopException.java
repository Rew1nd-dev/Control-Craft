package com.verr1.controlcraft.foundation.cimulink.game.exceptions;

public class EncloseLoopException extends RuntimeException{

    public EncloseLoopException() {
    }

    public EncloseLoopException(String s) {
        super(s);
    }

    public EncloseLoopException(String message, Throwable cause) {
        super(message, cause);
    }

    public EncloseLoopException(Throwable cause) {
        super(cause);
    }
}

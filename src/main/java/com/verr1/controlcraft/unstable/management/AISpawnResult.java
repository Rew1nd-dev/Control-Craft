package com.verr1.controlcraft.unstable.management;

public class AISpawnResult {
    public final long id;
    public final Status status;

    public AISpawnResult(long id, Status status) {
        this.id = id;
        this.status = status;
    }

    public AISpawnResult(long id) {
        this.id = id;
        this.status = id == -1 ? Status.FAILED : Status.SUCCESS;
    }

    public enum Status {
        SUCCESS,
        FAILED
    }
}

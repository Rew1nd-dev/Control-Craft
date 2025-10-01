package com.verr1.controlcraft.unstable.management;

public class AISpawnResult {
    public static final AISpawnResult FAILED = new AISpawnResult(-1, Status.FAILED);
    public final long id;
    public final Status status;

    public AISpawnResult(long id, Status status) {
        this.id = id;
        this.status = status;
    }

    public AISpawnResult(long id) {
        this(id, id == -1 ? Status.FAILED : Status.SUCCESS);
    }

    public enum Status {
        SUCCESS,
        FAILED
    }
}

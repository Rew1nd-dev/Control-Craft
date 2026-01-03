package com.verr1.controlcraft.unstable.management;

public class AISpawnResult {
    public static final AISpawnResult USE_UP = new AISpawnResult(-1, Status.USE_UP, RepairResult.DID_NOT_REPAIR);
    public static final AISpawnResult NOT_AN_AI = new AISpawnResult(-1, Status.NOT_AN_AI, RepairResult.DID_NOT_REPAIR);
    public static final AISpawnResult NOT_AVAILABLE = new AISpawnResult(-1, Status.NOT_AVAILABLE, RepairResult.DID_NOT_REPAIR);
    public static final AISpawnResult DELETED = new AISpawnResult(-1, Status.CAN_NOT_GET_SHIP, RepairResult.DID_NOT_REPAIR);

    public final long id;
    public final Status status;
    public final RepairResult repair;

    public AISpawnResult(long id, Status status, RepairResult repair) {
        this.id = id;
        this.status = status;
        this.repair = repair;
    }

    public AISpawnResult(long id) {
        this(id, Status.SUCCESS, RepairResult.SUCCESS);
    }

    public enum Status {
        SUCCESS,
        USE_UP,
        NOT_AN_AI,
        NOT_AVAILABLE,
        CAN_NOT_GET_SHIP,
        CAN_NOT_REPAIR

    }

    @Override
    public String toString() {
        return status.name();
    }
}

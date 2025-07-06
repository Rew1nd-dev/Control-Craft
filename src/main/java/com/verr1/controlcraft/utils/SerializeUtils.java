package com.verr1.controlcraft.utils;

import com.verr1.controlcraft.ControlCraft;
import com.verr1.controlcraft.foundation.data.NetworkKey;
import com.verr1.controlcraft.foundation.data.constraint.ConnectContext;
import com.verr1.controlcraft.foundation.data.links.BlockPort;
import kotlin.Pair;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtIo;
import org.jetbrains.annotations.NotNull;
import org.joml.Quaterniond;
import org.joml.Quaterniondc;
import org.joml.Vector3d;
import org.joml.Vector3dc;

import org.valkyrienskies.core.apigame.joints.*;

import java.io.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public class SerializeUtils {
    public static HashMap<Class<?>, Serializer<?>> EnumSerializerCache = new HashMap<>();

    public static Serializer<Double> DOUBLE = of(SerializeUtils::ofDouble, tag -> tag.getDouble("value"));
    public static Serializer<Float> FLOAT = of(SerializeUtils::ofFloat, tag -> tag.getFloat("value"));
    public static Serializer<Integer> INT = of(SerializeUtils::ofInt, tag -> tag.getInt("value"));
    public static Serializer<Long> LONG = of(SerializeUtils::ofLong, tag -> tag.getLong("value"));
    public static Serializer<Boolean> BOOLEAN = of(SerializeUtils::ofBoolean, tag -> tag.getBoolean("value"));
    public static Serializer<String> STRING = of(SerializeUtils::ofString, tag -> tag.getString("value"));
    public static Serializer<CompoundTag> UNIT = of(tag -> tag, tag -> tag);

    public static Serializer<BlockPort> BLOCK_PORT = SerializeUtils.of(
            BlockPort::serialize,
            BlockPort::deserialize
    );

    public static Serializer<BlockPos> BLOCK_POS = SerializeUtils.of(
            bp -> LONG.serialize(bp.asLong()),
            l -> BlockPos.of(LONG.deserialize(l))
    );

    public static Serializer<Vector3dc> VECTOR3DC = of(
            vec -> {
                CompoundTag tag = new CompoundTag();
                tag.putDouble("x", vec.x());
                tag.putDouble("y", vec.y());
                tag.putDouble("z", vec.z());
                return tag;
            },
            tag -> new Vector3d(tag.getDouble("x"), tag.getDouble("y"), tag.getDouble("z"))
    );
    public static Serializer<Quaterniondc> QUATERNION4DC = of(
            quat -> {
                CompoundTag tag = new CompoundTag();
                tag.putDouble("x", quat.x());
                tag.putDouble("y", quat.y());
                tag.putDouble("z", quat.z());
                tag.putDouble("w", quat.w());
                return tag;
            },
            tag -> new Quaterniond(tag.getDouble("x"), tag.getDouble("y"), tag.getDouble("z"), tag.getDouble("w"))
    );

    public static Serializer<Vector3d> VECTOR3D = of(
            vec -> {
                CompoundTag tag = new CompoundTag();
                tag.putDouble("x", vec.x());
                tag.putDouble("y", vec.y());
                tag.putDouble("z", vec.z());
                return tag;
            },
            tag -> new Vector3d(tag.getDouble("x"), tag.getDouble("y"), tag.getDouble("z"))
    );
    public static Serializer<Quaterniond> QUATERNION4D = of(
            quat -> {
                CompoundTag tag = new CompoundTag();
                tag.putDouble("x", quat.x());
                tag.putDouble("y", quat.y());
                tag.putDouble("z", quat.z());
                tag.putDouble("w", quat.w());
                return tag;
            },
            tag -> new Quaterniond(tag.getDouble("x"), tag.getDouble("y"), tag.getDouble("z"), tag.getDouble("w"))
    );

    public static<T, A, C extends Collection<T>> Serializer<Collection<T>> ofCollection(Serializer<T> elementSerializer, Collector<T, A, C> collector){
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull Collection<T> obj) {

                AtomicInteger count = new AtomicInteger();
                CompoundTagBuilder builder = new CompoundTagBuilder().withLong("count", (long) obj.size());
                obj.forEach(d -> builder.withCompound("element_" + count.getAndIncrement(), elementSerializer.serialize(d)));
                return builder.build();

            }

            @Override
            public @NotNull Collection<T> deserialize(CompoundTag tag) {
                A container = collector.supplier().get();
                long count = tag.getLong("count");
                for (long i = 0; i < count; i++) {
                    CompoundTag elementTag = tag.getCompound("element_" + i);
                    T element = elementSerializer.deserialize(elementTag);
                    collector.accumulator().accept(container, element);
                }
                return collector.finisher().apply(container);
            }
        };
    }

    public static <K, V> Serializer<HashMap<K, V>> ofMap(Serializer<K> keySerializer, Serializer<V> valueSerializer) {
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull HashMap<K, V> map) {
                CompoundTagBuilder builder = new CompoundTagBuilder();
                builder.withLong("count", (long) map.size());
                int index = 0;
                for (Map.Entry<K, V> entry : map.entrySet()) {
                    CompoundTag keyTag = keySerializer.serialize(entry.getKey());
                    CompoundTag valueTag = valueSerializer.serialize(entry.getValue());
                    builder.withCompound("key_" + index, keyTag);
                    builder.withCompound("value_" + index, valueTag);
                    index++;
                }
                return builder.build();
            }

            @Override
            public @NotNull HashMap<K, V> deserialize(CompoundTag tag) {
                long count = tag.getLong("count");
                HashMap<K, V> map = new HashMap<>();
                for (long i = 0; i < count; i++) {
                    CompoundTag keyTag = tag.getCompound("key_" + i);
                    CompoundTag valueTag = tag.getCompound("value_" + i);
                    K key = keySerializer.deserialize(keyTag);
                    V value = valueSerializer.deserialize(valueTag);
                    map.put(key, value);
                }
                return map;
            }
        };
    }

    public static <V> Serializer<Set<V>> ofSet(Serializer<V> elementSerializer) {
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull Set<V> set) {
                CompoundTagBuilder builder = new CompoundTagBuilder();
                builder.withLong("count", (long) set.size());
                int index = 0;
                for (V element : set) {
                    CompoundTag elementTag = elementSerializer.serialize(element);
                    builder.withCompound("element_" + index, elementTag);
                    index++;
                }
                return builder.build();
            }

            @Override
            public @NotNull Set<V> deserialize(CompoundTag tag) {
                long count = tag.getLong("count");
                Set<V> set = new HashSet<>();
                for (long i = 0; i < count; i++) {
                    CompoundTag elementTag = tag.getCompound("element_" + i);
                    V element = elementSerializer.deserialize(elementTag);
                    set.add(element);
                }
                return set;
            }
        };
    }

    public static <F, S> Serializer<Pair<F, S>> ofPair(Serializer<F> firstSerializer, Serializer<S> secondSerializer) {
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull Pair<F, S> pair) {
                CompoundTag tag = new CompoundTag();
                tag.put("first", firstSerializer.serialize(pair.getFirst()));
                tag.put("second", secondSerializer.serialize(pair.getSecond()));
                return tag;
            }

            @Override
            public @NotNull Pair<F, S> deserialize(CompoundTag tag) {
                F first = firstSerializer.deserialize(tag.getCompound("first"));
                S second = secondSerializer.deserialize(tag.getCompound("second"));
                return new Pair<>(first, second);
            }
        };
    }

    public static <T> Serializer<Pair<T, T>> ofPair(Serializer<T> serializer) {
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull Pair<T, T> pair) {
                CompoundTag tag = new CompoundTag();
                tag.put("first", serializer.serialize(pair.getFirst()));
                tag.put("second", serializer.serialize(pair.getSecond()));
                return tag;
            }

            @Override
            public @NotNull Pair<T, T> deserialize(CompoundTag tag) {
                T first = serializer.deserialize(tag.getCompound("first"));
                T second = serializer.deserialize(tag.getCompound("second"));
                return new Pair<>(first, second);
            }
        };
    }

    /**
     * Creates a Serializer for List<V> using the provided element Serializer.
     *
     * @param elementSerializer the Serializer for the elements of the List
     * @param <V> the type of elements in the List
     * @return a Serializer for List<V>
     */
    public static <V> Serializer<List<V>> ofList(Serializer<V> elementSerializer) {
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull List<V> list) {
                CompoundTagBuilder builder = new CompoundTagBuilder();
                builder.withLong("count", (long) list.size());
                for (int i = 0; i < list.size(); i++) {
                    V element = list.get(i);
                    CompoundTag elementTag = elementSerializer.serialize(element);
                    builder.withCompound("element_" + i, elementTag);
                }
                return builder.build();
            }

            @Override
            public @NotNull List<V> deserialize(CompoundTag tag) {
                long count = tag.getLong("count");
                List<V> list = new ArrayList<>();
                for (long i = 0; i < count; i++) {
                    CompoundTag elementTag = tag.getCompound("element_" + i);
                    V element = elementSerializer.deserialize(elementTag);
                    list.add(element);
                }
                return list;
            }
        };
    }



    @SuppressWarnings("unchecked") // It's checked
    public static<T extends Enum<?>> Serializer<T> ofEnum(Class<T> enumClazz){
        return (Serializer<T>)EnumSerializerCache.computeIfAbsent(
                enumClazz,
                clazz_ ->
                        new Serializer<T>() {
                            final Class<T> clazz = enumClazz;

                            @Override
                            public CompoundTag serialize(@NotNull T obj) {
                                return INT.serialize(obj.ordinal());
                            }

                            @Override
                            public @NotNull T deserialize(CompoundTag tag) {
                                int ordinal = INT.deserialize(tag);
                                T validValue = clazz.getEnumConstants()[0];
                                try{
                                    validValue = clazz.getEnumConstants()[ordinal];
                                }catch (IndexOutOfBoundsException e){
                                    ControlCraft.LOGGER.error("receive ordinal: {}, but class {} does not contain that much elements", ordinal, clazz);
                                }
                                return validValue;
                            }
                        }
                );

    }



    public static Serializer<VSJointPose> JOINT_POSE = of(
            pose -> {
                CompoundTag tag = new CompoundTag();
                tag.put("p", VECTOR3DC.serialize(pose.getPos()));
                tag.put("q", QUATERNION4DC.serialize(pose.getRot()));
                return tag;
            },
            tag -> new VSJointPose(
                    VECTOR3DC.deserialize(tag.getCompound("p")),
                    QUATERNION4DC.deserialize(tag.getCompound("q")))
    );

    public static Serializer<ConnectContext> CONNECT_CONTEXT = of(
            context -> {
                CompoundTag tag = new CompoundTag();
                tag.put("self", JOINT_POSE.serialize(context.self()));
                tag.put("comp", JOINT_POSE.serialize(context.comp()));
                tag.put("dirty", BOOLEAN.serialize(context.isDirty()));
                return tag;
            },
            tag -> new ConnectContext(
                    JOINT_POSE.deserialize(tag.getCompound("self")),
                    JOINT_POSE.deserialize(tag.getCompound("comp")),
                    BOOLEAN.deserialize(tag.getCompound("dirty"))
                )
            );


    public static Serializer<VSJointMaxForceTorque> MAX_FORCE_TORQUE = of(
            max -> {
                CompoundTag tag = new CompoundTag();
                tag.put("max_force", FLOAT.serializeNullable(max.getMaxForce()));
                tag.put("max_torque", FLOAT.serializeNullable(max.getMaxTorque()));
                return tag;
            },
            tag -> {
                Float force = FLOAT.deserializeNullable(tag.getCompound("max_force"));
                Float torque = FLOAT.deserializeNullable(tag.getCompound("max_torque"));
                if(force == null || torque == null){
                    return null;
                }
                return new VSJointMaxForceTorque(force, torque);
            }
    );

    public static Serializer<VSD6Joint.AngularLimitPair> ANGULAR_LIMIT_PAIR = of(
            pair -> {
                CompoundTag tag = new CompoundTag();
                tag.put("min", FLOAT.serialize(pair.getLowerLimit()));
                tag.put("max", FLOAT.serialize(pair.getUpperLimit()));
                tag.put("restitution", FLOAT.serializeNullable(pair.getRestitution()));
                tag.put("bounce_threshold", FLOAT.serializeNullable(pair.getBounceThreshold()));
                tag.put("stiffness", FLOAT.serializeNullable(pair.getStiffness()));
                tag.put("damping", FLOAT.serializeNullable(pair.getDamping()));
                return tag;
            },
            tag -> new VSD6Joint.AngularLimitPair(
                    FLOAT.deserialize(tag.getCompound("min")),
                    FLOAT.deserialize(tag.getCompound("max")),
                    FLOAT.deserializeNullable(tag.getCompound("restitution")),
                    FLOAT.deserializeNullable(tag.getCompound("bounce_threshold")),
                    FLOAT.deserializeNullable(tag.getCompound("stiffness")),
                    FLOAT.deserializeNullable(tag.getCompound("damping"))
            )
    );

    public static Serializer<VSD6Joint.LinearLimitPair> LINEAR_LIMIT_PAIR = of(
            pair -> {
                CompoundTag tag = new CompoundTag();
                tag.put("min", FLOAT.serialize(pair.getLowerLimit()));
                tag.put("max", FLOAT.serialize(pair.getUpperLimit()));
                tag.put("restitution", FLOAT.serializeNullable(pair.getRestitution()));
                tag.put("bounce_threshold", FLOAT.serializeNullable(pair.getBounceThreshold()));
                tag.put("stiffness", FLOAT.serializeNullable(pair.getStiffness()));
                tag.put("damping", FLOAT.serializeNullable(pair.getDamping()));
                return tag;
            },
            tag -> new VSD6Joint.LinearLimitPair(
                    FLOAT.deserialize(tag.getCompound("min")),
                    FLOAT.deserialize(tag.getCompound("max")),
                    FLOAT.deserializeNullable(tag.getCompound("restitution")),
                    FLOAT.deserializeNullable(tag.getCompound("bounce_threshold")),
                    FLOAT.deserializeNullable(tag.getCompound("stiffness")),
                    FLOAT.deserializeNullable(tag.getCompound("damping"))
            )
    );

    public static Serializer<VSRevoluteJoint.VSRevoluteDriveVelocity> REVOLUTE_DRIVE_VELOCITY = of(
            drive -> {
                CompoundTag tag = new CompoundTag();
                tag.put("velocity", FLOAT.serialize(drive.getVelocity()));
                tag.put("auto_wake", BOOLEAN.serialize(drive.getAutoWake()));
                return tag;
            },
            tag -> new VSRevoluteJoint.VSRevoluteDriveVelocity(
                    FLOAT.deserialize(tag.getCompound("velocity")),
                    BOOLEAN.deserialize(tag.getCompound("auto_wake"))
            )
    );

    public static Serializer<VSJoint> JOINT = of(
            joint -> {
                CompoundTag tag = new CompoundTag();
                tag.put("ship_id_0", LONG.serialize(Optional.ofNullable(joint.getShipId0()).orElse(-1L)));
                tag.put("ship_id_1", LONG.serialize(Optional.ofNullable(joint.getShipId1()).orElse(-1L)));
                tag.put("pose_0", JOINT_POSE.serialize(joint.getPose0()));
                tag.put("pose_1", JOINT_POSE.serialize(joint.getPose1()));

                return tag;
            },
            tag -> null
    );

    public static Serializer<VSFixedJoint> FIXED_JOINT = of(
            joint -> {
                CompoundTag tag = new CompoundTag();
                tag.put("joint", JOINT.serialize(joint));
                tag.put("max_force_torque", MAX_FORCE_TORQUE.serializeNullable(joint.getMaxForceTorque()));
                return tag;
            },
            tag -> {
                CompoundTag commonJoint = tag.getCompound("joint");
                return new VSFixedJoint(
                        LONG.deserialize(commonJoint.getCompound("ship_id_0")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("joint").getCompound("pose_0")),
                        LONG.deserialize(commonJoint.getCompound("joint").getCompound("ship_id_1")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("joint").getCompound("pose_1")),
                        MAX_FORCE_TORQUE.deserializeNullable(tag.getCompound("max_force_torque"))
                );
            }
    );

    public static Serializer<VSPrismaticJoint> PRISMATIC_JOINT = of(
            joint -> {
                CompoundTag tag = new CompoundTag();
                tag.put("joint", JOINT.serialize(joint));
                tag.put("max_force_torque", MAX_FORCE_TORQUE.serializeNullable(joint.getMaxForceTorque()));
                tag.put("linear_limit_pair", LINEAR_LIMIT_PAIR.serializeNullable(joint.getLinearLimitPair()));
                return tag;
            },
            tag -> {
                CompoundTag commonJoint = tag.getCompound("joint");
                return new VSPrismaticJoint(
                        LONG.deserialize(commonJoint.getCompound("ship_id_0")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("pose_0")),
                        LONG.deserialize(commonJoint.getCompound("ship_id_1")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("pose_1")),
                        MAX_FORCE_TORQUE.deserializeNullable(tag.getCompound("max_force_torque")),
                        LINEAR_LIMIT_PAIR.deserializeNullable(tag.getCompound("linear_limit_pair"))
                );
            }
    );

    public static Serializer<VSDistanceJoint> DISTANCE_JOINT = of(
            joint -> {
                CompoundTag tag = new CompoundTag();
                tag.put("joint", JOINT.serialize(joint));
                tag.put("max_force_torque", MAX_FORCE_TORQUE.serializeNullable(joint.getMaxForceTorque()));
                tag.put("min", FLOAT.serializeNullable(joint.getMinDistance()));
                tag.put("max", FLOAT.serializeNullable(joint.getMinDistance()));
                tag.put("tolerance", FLOAT.serializeNullable(joint.getTolerance()));
                tag.put("stiffness", FLOAT.serializeNullable(joint.getStiffness()));
                tag.put("damping", FLOAT.serializeNullable(joint.getDamping()));
                return tag;
            },
            tag -> {
                CompoundTag commonJoint = tag.getCompound("joint");
                return new VSDistanceJoint(
                        LONG.deserialize(commonJoint.getCompound("ship_id_0")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("pose_0")),
                        LONG.deserialize(commonJoint.getCompound("ship_id_1")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("pose_1")),
                        MAX_FORCE_TORQUE.deserializeNullable(tag.getCompound("max_force_torque")),
                        FLOAT.deserialize(tag.getCompound("min")),
                        FLOAT.deserialize(tag.getCompound("max")),
                        FLOAT.deserializeNullable(tag.getCompound("tolerance")),
                        FLOAT.deserializeNullable(tag.getCompound("stiffness")),
                        FLOAT.deserializeNullable(tag.getCompound("damping"))
                );
            }
    );

    public static Serializer<VSRevoluteJoint> REVOLUTE_JOINT = of(
            joint -> {
                CompoundTag tag = new CompoundTag();
                tag.put("joint", JOINT.serialize(joint));
                tag.put("max_force_torque", MAX_FORCE_TORQUE.serializeNullable(joint.getMaxForceTorque()));
                tag.put("angular_limit_pair", ANGULAR_LIMIT_PAIR.serializeNullable(joint.getAngularLimitPair()));
                tag.put("drive_velocity", REVOLUTE_DRIVE_VELOCITY.serializeNullable(joint.getDriveVelocity()));
                tag.put("drive_force_limit", FLOAT.serializeNullable(joint.getDriveForceLimit()));
                tag.put("drive_gear_ratio", FLOAT.serializeNullable(joint.getDriveGearRatio()));
                tag.put("drive_free_spin", BOOLEAN.serializeNullable(joint.getDriveFreeSpin()));
                return tag;
            },
            tag -> {
                CompoundTag commonJoint = tag.getCompound("joint");
                return new VSRevoluteJoint(
                        LONG.deserialize(commonJoint.getCompound("ship_id_0")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("pose_0")),
                        LONG.deserialize(commonJoint.getCompound("ship_id_1")),
                        JOINT_POSE.deserialize(commonJoint.getCompound("pose_1")),
                        MAX_FORCE_TORQUE.deserializeNullable(tag.getCompound("max_force_torque")),
                        ANGULAR_LIMIT_PAIR.deserializeNullable(tag.getCompound("angular_limit_pair")),
                        REVOLUTE_DRIVE_VELOCITY.deserializeNullable(tag.getCompound("drive_velocity")),
                        FLOAT.deserializeNullable(tag.getCompound("drive_force_limit")),
                        FLOAT.deserializeNullable(tag.getCompound("drive_gear_ratio")),
                        BOOLEAN.deserializeNullable(tag.getCompound("drive_free_spin"))
                );
            }
    );


    public static <T> Serializer<T> of(Function<T, CompoundTag> serializer, Function<CompoundTag, T> deserializer){
        return new Serializer<>() {
            @Override
            public CompoundTag serialize(@NotNull T obj) {
                return serializer.apply(obj);
            }

            @Override
            public @NotNull T deserialize(CompoundTag tag) {
                return deserializer.apply(tag);
            }
        };
    }

    public static CompoundTag ofDouble(double d){
        CompoundTag tag = new CompoundTag();
        tag.putDouble("value", d);
        return tag;
    }

    public static CompoundTag ofFloat(float f){
        CompoundTag tag = new CompoundTag();
        tag.putFloat("value", f);
        return tag;
    }

    public static CompoundTag ofInt(int i){
        CompoundTag tag = new CompoundTag();
        tag.putInt("value", i);
        return tag;
    }

    public static CompoundTag ofBoolean(boolean b){
        CompoundTag tag = new CompoundTag();
        tag.putBoolean("value", b);
        return tag;
    }

    public static CompoundTag ofString(String s){
        CompoundTag tag = new CompoundTag();
        tag.putString("value", s);
        return tag;
    }

    public static CompoundTag ofLong(long l){
        CompoundTag tag = new CompoundTag();
        tag.putLong("value", l);
        return tag;
    }


    public static class ReadWriter<T> extends ReadWriteExecutor {
        private final Supplier<T> supplier;
        private final Consumer<T> consumer;
        private final Serializer<T> serializer;
        private final NetworkKey key;

        private ReadWriter(Supplier<T> supplier, Consumer<T> consumer, Serializer<T> serializer, NetworkKey key){
            super(
                    tag -> consumer.accept(serializer.deserialize(tag)),
                    tag -> tag.put(key.getSerializedName(), serializer.serialize(supplier.get())),
                    key
            );
            this.supplier = supplier;
            this.consumer = consumer;
            this.serializer = serializer;
            this.key = key;

        }

        public static <T> ReadWriter<T> of(Supplier<T> supplier, Consumer<T> consumer, Serializer<T> serializer, NetworkKey key){
            return new ReadWriter<>(
                    supplier, consumer, serializer, key
            );
        }


        public NetworkKey getKey() {
            return key;
        }

        public void onReadWithKey(CompoundTag tag){
            if(!tag.contains(key.getSerializedName()))return;
            consumer.accept(serializer.deserialize(tag.getCompound(key.getSerializedName())));
        }

        public void onReadDefault(CompoundTag tag){
            // if(!tag.contains(key))return;
            consumer.accept(serializer.deserialize(tag.getCompound(key.getSerializedName())));
        }
 
        public void onWriteDefault(CompoundTag tag){
            tag.put(key.getSerializedName(), serializer.serialize(supplier.get()));
        }


    }

    public static class ReadWriteExecutor{
        Consumer<CompoundTag> onRead;
        Consumer<CompoundTag> onWrite;
        NetworkKey key;


        public NetworkKey getKey() {
            return key;
        }

        protected ReadWriteExecutor(
                Consumer<CompoundTag> onRead,
                Consumer<CompoundTag> onWrite,
                NetworkKey key
        ) {
            this.onRead = onRead;
            this.onWrite = onWrite;
            this.key = key;
        }


        public static ReadWriteExecutor of(Consumer<CompoundTag> onRead, Consumer<CompoundTag> onWrite, NetworkKey key){
            return new ReadWriteExecutor(
                    onRead, onWrite, key
            );
        }

        public void onReadWithKey(CompoundTag t){
            if(!t.contains(key.getSerializedName()))return;
            onRead.accept(t.getCompound(key.getSerializedName()));
        }

        public void onReadDefault(CompoundTag t){
            onRead.accept(t);
        }

        public void onWriteDefault(CompoundTag t){
            onWrite.accept(t);
        }

        public void onWriteWithKey(CompoundTag t){
            CompoundTag tag = new CompoundTag();
            onWrite.accept(tag);
            t.put(key.getSerializedName(), tag);
        }

    }

    public static byte[] compress(CompoundTag tag) throws IOException {
        ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
        try (GZIPOutputStream gzipOutput = new GZIPOutputStream(byteOutput);
             DataOutputStream dataOutput = new DataOutputStream(gzipOutput)) {

            NbtIo.write(tag, dataOutput); // 序列化NBT
        }
        return byteOutput.toByteArray();
    }

    public static CompoundTag decompress(byte[] compressedData) throws IOException {
        try (GZIPInputStream gzipInput = new GZIPInputStream(new ByteArrayInputStream(compressedData));
             DataInputStream dataInput = new DataInputStream(gzipInput)) {

            return NbtIo.read(dataInput); // 反序列化NBT
        }
    }

}

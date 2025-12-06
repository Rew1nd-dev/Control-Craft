package com.verr1.controlcraft.foundation.network.executors;

import com.verr1.controlcraft.content.blocks.NetworkBlockEntity;
import com.verr1.controlcraft.foundation.data.NetworkKey;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class AsyncLazySynchronizer {

    private final NetworkBlockEntity delegate;
    private final Map<NetworkKey, BooleanLatch> keys = new ConcurrentHashMap<>();
    private final AsyncLazyExecutor executor = new AsyncLazyExecutor(this::doSynchronization);

    public AsyncLazySynchronizer(NetworkBlockEntity delegate) {
        this.delegate = delegate;
        executor.setCooldownMillis(45);
    }

    public void doSynchronization(){
        delegate.syncForNear(true, toUpdate());
    }

    private NetworkKey[] toUpdate(){
        List<NetworkKey> keys = new ArrayList<>();
        for(Map.Entry<NetworkKey, BooleanLatch> entry : this.keys.entrySet()){
            if(entry.getValue().poll()){
                keys.add(entry.getKey());
            }
        }
        return keys.toArray(NetworkKey[]::new);
    }

    public void queueUpdate(NetworkKey... key){
        for(var k : key){
            keys.computeIfAbsent(k, $ -> new BooleanLatch()).set();
        }
        executor.executeAsync();
    }



    public void tick(){
        executor.execute();
    }

    public void setCooldownMillis(long millis){
        executor.setCooldownMillis(millis);
    }

    private static class BooleanLatch{
        private boolean value = false;

        public boolean poll(){
            boolean val = peek();
            value = false;
            return val;
        }

        public void set(){
            value = true;
        }

        public boolean peek(){
            return value;
        }
    }

}

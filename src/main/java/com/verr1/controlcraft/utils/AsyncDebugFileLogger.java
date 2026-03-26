package com.verr1.controlcraft.utils;

import com.verr1.controlcraft.ControlCraft;
import net.minecraftforge.fml.loading.FMLPaths;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

public final class AsyncDebugFileLogger {

    private static final Path ROOT_DIRECTORY = FMLPaths.GAMEDIR.get()
            .resolve("controlcraft-debug")
            .resolve("lua");
    private static final int MAX_PENDING_OPERATIONS = 8192;
    private static final int MAX_BATCH_SIZE = 128;

    private static final LinkedBlockingQueue<WriteOperation> QUEUE = new LinkedBlockingQueue<>(MAX_PENDING_OPERATIONS);
    private static final AtomicBoolean RUNNING = new AtomicBoolean(false);
    private static final AtomicLong DROPPED_OPERATIONS = new AtomicLong(0);
    private static final Object LIFECYCLE_LOCK = new Object();

    private static @Nullable Thread workerThread;

    private AsyncDebugFileLogger() {
    }

    public static void start() {
        synchronized (LIFECYCLE_LOCK) {
            if (RUNNING.get()) {
                return;
            }

            RUNNING.set(true);
            Thread worker = new Thread(AsyncDebugFileLogger::runWorkerLoop, "ControlCraft-DebugFileLogger");
            worker.setDaemon(true);
            worker.start();
            workerThread = worker;
        }
    }

    public static void stop() {
        Thread worker;
        synchronized (LIFECYCLE_LOCK) {
            if (!RUNNING.get()) {
                return;
            }

            RUNNING.set(false);
            worker = workerThread;
            if (worker != null) {
                worker.interrupt();
            }
            workerThread = null;
        }

        if (worker != null) {
            try {
                worker.join(3000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
    }

    public static boolean appendLine(@Nullable String fileName, String message) {
        start();
        return enqueue(new AppendOperation(normalizeFileName(fileName), Objects.requireNonNullElse(message, "")));
    }

    public static boolean resetFile(@Nullable String fileName) {
        start();
        return enqueue(new ResetOperation(normalizeFileName(fileName)));
    }

    public static Path getRootDirectory() {
        return ROOT_DIRECTORY;
    }

    public static Path resolvePath(@Nullable String fileName) {
        return ROOT_DIRECTORY.resolve(normalizeFileName(fileName));
    }

    public static String normalizeFileName(@Nullable String rawName) {
        String candidate = rawName == null ? "" : rawName.trim();
        if (candidate.isEmpty()) {
            return "debug.log";
        }

        StringBuilder sanitized = new StringBuilder(candidate.length());
        for (int i = 0; i < candidate.length(); i++) {
            char c = candidate.charAt(i);
            if (Character.isLetterOrDigit(c) || c == '.' || c == '_' || c == '-') {
                sanitized.append(c);
            } else {
                sanitized.append('_');
            }
        }

        String normalized = sanitized.toString();
        while (normalized.startsWith(".")) {
            normalized = normalized.substring(1);
        }

        if (normalized.isBlank()) {
            return "debug.log";
        }

        if (!normalized.contains(".")) {
            normalized = normalized + ".log";
        }

        return normalized;
    }

    private static boolean enqueue(WriteOperation operation) {
        boolean accepted = QUEUE.offer(operation);
        if (!accepted) {
            long dropped = DROPPED_OPERATIONS.incrementAndGet();
            if (dropped == 1 || dropped % 100 == 0) {
                ControlCraft.LOGGER.warn("Async debug log queue is full, dropped {} log operations.", dropped);
            }
        }
        return accepted;
    }

    private static void runWorkerLoop() {
        Map<Path, BufferedWriter> writers = new HashMap<>();
        List<WriteOperation> batch = new ArrayList<>(MAX_BATCH_SIZE);

        try {
            while (RUNNING.get() || !QUEUE.isEmpty()) {
                WriteOperation first = QUEUE.poll(500, TimeUnit.MILLISECONDS);
                if (first == null) {
                    flushAll(writers);
                    continue;
                }

                batch.add(first);
                QUEUE.drainTo(batch, MAX_BATCH_SIZE - 1);

                for (WriteOperation operation : batch) {
                    execute(operation, writers);
                }

                flushAll(writers);
                batch.clear();
            }
        } catch (InterruptedException ignored) {
            Thread.currentThread().interrupt();
        } finally {
            WriteOperation remaining;
            while ((remaining = QUEUE.poll()) != null) {
                execute(remaining, writers);
            }
            flushAll(writers);
            closeAll(writers);
        }
    }

    private static void execute(WriteOperation operation, Map<Path, BufferedWriter> writers) {
        try {
            if (operation instanceof AppendOperation append) {
                Path path = resolvePath(append.fileName());
                BufferedWriter writer = writers.computeIfAbsent(path, AsyncDebugFileLogger::openWriter);
                writer.write(append.message());
                if (!append.message().endsWith("\n")) {
                    writer.newLine();
                }
                return;
            }

            if (operation instanceof ResetOperation reset) {
                Path path = resolvePath(reset.fileName());
                BufferedWriter existing = writers.remove(path);
                closeQuietly(existing);
                Files.createDirectories(path.getParent());
                try (BufferedWriter ignored = Files.newBufferedWriter(
                        path,
                        StandardCharsets.UTF_8,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.TRUNCATE_EXISTING,
                        StandardOpenOption.WRITE
                )) {
                    // Intentionally empty: opening with TRUNCATE_EXISTING is enough.
                }
            }
        } catch (Exception e) {
            ControlCraft.LOGGER.warn("Failed to write async debug log '{}': {}", operation.fileName(), e.toString());
        }
    }

    private static BufferedWriter openWriter(Path path) {
        try {
            Files.createDirectories(path.getParent());
            return Files.newBufferedWriter(
                    path,
                    StandardCharsets.UTF_8,
                    StandardOpenOption.CREATE,
                    StandardOpenOption.APPEND,
                    StandardOpenOption.WRITE
            );
        } catch (IOException e) {
            throw new IllegalStateException("Unable to open debug log file " + path, e);
        }
    }

    private static void flushAll(Map<Path, BufferedWriter> writers) {
        for (BufferedWriter writer : writers.values()) {
            try {
                writer.flush();
            } catch (IOException e) {
                ControlCraft.LOGGER.warn("Failed to flush async debug log writer: {}", e.toString());
            }
        }
    }

    private static void closeAll(Map<Path, BufferedWriter> writers) {
        for (BufferedWriter writer : writers.values()) {
            closeQuietly(writer);
        }
        writers.clear();
    }

    private static void closeQuietly(@Nullable BufferedWriter writer) {
        if (writer == null) {
            return;
        }

        try {
            writer.close();
        } catch (IOException e) {
            ControlCraft.LOGGER.warn("Failed to close async debug log writer: {}", e.toString());
        }
    }

    private sealed interface WriteOperation permits AppendOperation, ResetOperation {
        String fileName();
    }

    private record AppendOperation(String fileName, String message) implements WriteOperation {
    }

    private record ResetOperation(String fileName) implements WriteOperation {
    }
}

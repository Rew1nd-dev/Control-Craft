package luaj;

import java.util.concurrent.*;

public class FutureGetBehaviorTest {
    
    public static void main(String[] args) throws Exception {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        
        System.out.println("=== 测试1：任务在超时前完成 ===");
        testTaskCompletesBeforeTimeout(executor);
        
        System.out.println("\n=== 测试2：任务在超时后完成 ===");
        testTaskCompletesAfterTimeout(executor);
        
        System.out.println("\n=== 测试3：任务立即完成 ===");
        testTaskCompletesImmediately(executor);
        
        executor.shutdown();
    }
    
    static void testTaskCompletesBeforeTimeout(ExecutorService executor) throws Exception {
        long startTime = System.currentTimeMillis();
        
        Future<String> future = executor.submit(() -> {
            Thread.sleep(10); // 任务执行10ms
            return "任务完成";
        });
        
        Thread.sleep(5); // 让任务先开始执行
        
        try {
            String result = future.get(50, TimeUnit.MILLISECONDS); // 超时设置50ms
            long elapsed = System.currentTimeMillis() - startTime;
            System.out.println("结果: " + result);
            System.out.println("实际等待时间: " + elapsed + "ms (约10ms，不是50ms)");
        } catch (TimeoutException e) {
            System.out.println("不应该走到这里");
        }
    }
    
    static void testTaskCompletesAfterTimeout(ExecutorService executor) throws Exception {
        long startTime = System.currentTimeMillis();
        
        Future<String> future = executor.submit(() -> {
            Thread.sleep(100); // 任务执行100ms
            return "任务完成";
        });
        
        try {
            String result = future.get(20, TimeUnit.MILLISECONDS); // 超时设置20ms
            System.out.println("不应该走到这里");
        } catch (TimeoutException e) {
            long elapsed = System.currentTimeMillis() - startTime;
            System.out.println("捕获TimeoutException");
            System.out.println("实际等待时间: " + elapsed + "ms (约20ms)");
            
            // 任务是否还在运行？
            System.out.println("任务是否取消: " + future.isCancelled());
            System.out.println("任务是否完成: " + future.isDone());
        }
        
        // 等待看任务是否会继续完成
        Thread.sleep(150);
        System.out.println("任务是否完成(150ms后): " + future.isDone());
    }
    
    static void testTaskCompletesImmediately(ExecutorService executor) throws Exception {
        long startTime = System.currentTimeMillis();
        
        Future<String> future = executor.submit(() -> {
            // 立即返回，不等待
            return "立即完成的任务";
        });
        
        // 立即提交一个任务，确保之前队列中的任务已完成
        Thread.sleep(10);
        
        try {
            String result = future.get(100, TimeUnit.MILLISECONDS); // 超时设置100ms
            long elapsed = System.currentTimeMillis() - startTime;
            System.out.println("结果: " + result);
            System.out.println("实际等待时间: " + elapsed + "ms (接近0ms，不是100ms)");
        } catch (TimeoutException e) {
            System.out.println("不应该走到这里");
        }
    }
}
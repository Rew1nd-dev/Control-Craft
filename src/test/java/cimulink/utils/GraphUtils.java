package cimulink.utils;

import java.util.*;
import java.util.function.Predicate;

public class GraphUtils {
    // Temporal Are 0-ordered
    public static Map<String, Integer> calculateOrders(
            Map<String, List<String>> graph,
            Predicate<String> isEnd,
            Predicate<String> zeroOrderVertices // true -> is zero
    ) {
        // Step 1: Build in-degree map
        Map<String, Integer> inDegree = new HashMap<>();
        for (String node : graph.keySet()) {
            inDegree.putIfAbsent(node, 0);
        }
        for (String node : graph.keySet()) {
            for (String neighbor : graph.get(node)) {
                inDegree.put(neighbor, inDegree.getOrDefault(neighbor, 0) + 1);
            }
        }

        // Step 2: Initialize order map
        Map<String, Integer> order = new HashMap<>();
        for (String node : graph.keySet()) {
            if (isEnd.test(node)) {
                order.put(node, 0); // End vertices have order 0
            } else {
                order.put(node, -1); // Uncomputed vertices marked as -1
            }
        }

        // Step 3: Initialize queue with known order-0 vertices
        Queue<String> queue = new LinkedList<>();
        for (String node : graph.keySet()) {
            if (isEnd.test(node) || zeroOrderVertices.test(node)) {
                queue.offer(node);
                if (!isEnd.test(node) && order.get(node) == -1) {
                    order.put(node, 0); // Non-end vertices with known order 0
                }
            }
        }

        // Step 4: BFS to compute orders
        while (!queue.isEmpty()) {
            String current = queue.poll();
            if (!graph.containsKey(current)) continue; // No outgoing edges

            for (String neighbor : graph.get(current)) {
                if (!isEnd.test(neighbor)) { // Only process non-end neighbors
                    int newOrder = order.get(current) + 1;
                    int currentOrder = order.get(neighbor);
                    if (currentOrder == -1 || newOrder > currentOrder) {
                        order.put(neighbor, newOrder); // Update to max order
                    }
                    inDegree.put(neighbor, inDegree.get(neighbor) - 1);
                    if (inDegree.get(neighbor) == 0) {
                        queue.offer(neighbor); // All dependencies resolved
                    }
                }
            }
        }

        // Step 5: Verify all vertices have been assigned an order
        for (String node : order.keySet()) {
            if (order.get(node) == -1) {
                throw new IllegalStateException("Unreachable vertex or invalid cycle detected: " + node);
            }
        }

        return order;
    }


    // Temporal Are Max-Ordered
    public static Map<String, Integer> calculateOrders2(
            Map<String, List<String>> graph,
            Predicate<String> isEnd,
            Predicate<String> zeroOrderVertices) {

        // Step 1: Build in-degree map
        Map<String, Integer> inDegree = new HashMap<>();
        for (String node : graph.keySet()) {
            inDegree.putIfAbsent(node, 0);
        }
        for (String node : graph.keySet()) {
            for (String neighbor : graph.get(node)) {
                inDegree.put(neighbor, inDegree.getOrDefault(neighbor, 0) + 1);
            }
        }

        // Step 2: Initialize order map
        Map<String, Integer> order = new HashMap<>();
        for (String node : graph.keySet()) {
            order.put(node, -1); // All vertices marked as -1 initially
        }

        // Step 3: Initialize queue with known order-0 non-end vertices
        Queue<String> queue = new LinkedList<>();
        for (String node : graph.keySet()) {
            if (zeroOrderVertices.test(node) && !isEnd.test(node)) {
                queue.offer(node);
                order.put(node, 0); // Set known zero order non-end vertices to 0
            }
        }

        // Step 4: BFS to compute orders
        while (!queue.isEmpty()) {
            String current = queue.poll();
            if (!graph.containsKey(current)) continue; // No outgoing edges

            for (String neighbor : graph.get(current)) {
                if (!isEnd.test(neighbor)) { // Only process non-end neighbors
                    int newOrder = order.get(current) + 1;
                    int currentOrder = order.get(neighbor);
                    if (currentOrder == -1 || newOrder > currentOrder) {
                        order.put(neighbor, newOrder); // Update to max order
                    }
                    inDegree.put(neighbor, inDegree.get(neighbor) - 1);
                    if (inDegree.get(neighbor) == 0) {
                        queue.offer(neighbor); // All dependencies resolved
                    }
                }
            }
        }

        // Step 5: Calculate orders for end vertices
        for (String node : graph.keySet()) {
            if (isEnd.test(node)) {
                int maxOrder = -1;
                for (String prev : getIncomingNodes(graph, node)) {
                    if (order.get(prev) != -1) {
                        maxOrder = Math.max(maxOrder, order.get(prev));
                    }
                }
                order.put(node, maxOrder + 1);
            }
        }

        // Step 6: Verify all vertices have been assigned an order
        for (String node : order.keySet()) {
            if (order.get(node) == -1) {
                throw new IllegalStateException("Unreachable vertex or invalid cycle detected: " + node);
            }
        }

        return order;
    }

    // Helper method to get incoming nodes
    private static List<String> getIncomingNodes(Map<String, List<String>> graph, String node) {
        List<String> incoming = new ArrayList<>();
        for (String key : graph.keySet()) {
            if (graph.get(key).contains(node)) {
                incoming.add(key);
            }
        }
        return incoming;
    }

}

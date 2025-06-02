package cimulink.v3.utils;

import java.util.*;

public class GraphUtils {

    public static <T> List<T> TopologySort(Map<T, Set<T>> graph) {
        // Step 1: Initialize in-degree map
        Map<T, Integer> inDegree = new HashMap<>();
        for (T node : graph.keySet()) {
            inDegree.put(node, 0);
        }
        for (Set<T> neighbors : graph.values()) {
            for (T neighbor : neighbors) {
                inDegree.put(neighbor, inDegree.getOrDefault(neighbor, 0) + 1);
            }
        }

        // Step 2: Initialize queue with nodes having in-degree 0
        Queue<T> queue = new LinkedList<>();
        for (Map.Entry<T, Integer> entry : inDegree.entrySet()) {
            if (entry.getValue() == 0) {
                queue.add(entry.getKey());
            }
        }

        // Step 3: Perform topological sort
        List<T> sortedList = new ArrayList<>();
        while (!queue.isEmpty()) {
            T current = queue.poll();
            sortedList.add(current);

            // Reduce in-degree of neighbors
            if (graph.containsKey(current)) {
                for (T neighbor : graph.get(current)) {
                    inDegree.put(neighbor, inDegree.get(neighbor) - 1);
                    if (inDegree.get(neighbor) == 0) {
                        queue.add(neighbor);
                    }
                }
            }
        }

        // Step 4: Check for cycles
        if (sortedList.size() < inDegree.size()) {
            throw new IllegalArgumentException("Graph contains a cycle, topological sort not possible.");
        }

        return sortedList;
    }
}
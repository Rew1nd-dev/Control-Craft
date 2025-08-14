package com.verr1.controlcraft.unstable.util;

import java.util.List;

public class BinarySearch {

    public static int maxIndexLessThan(List<Double> arr, double v) {
        if (arr == null || arr.isEmpty() || arr.get(0) >= v) {
            return -1; // 没有元素小于v
        }
        if (arr.get(arr.size() - 1) < v) {
            return arr.size() - 1; // 所有元素都小于v
        }

        int left = 0;
        int right = arr.size() - 1;
        int result = -1;

        while (left <= right) {
            int mid = left + (right - left) / 2; // 防止溢出
            if (arr.get(mid) < v) {
                result = mid; // 记录当前满足条件的索引
                left = mid + 1; // 尝试找更大的索引
            } else {
                right = mid - 1; // 目标值在左侧
            }
        }

        return result;
    }
}

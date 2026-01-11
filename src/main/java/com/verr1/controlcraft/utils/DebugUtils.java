package com.verr1.controlcraft.utils;

import java.io.PrintWriter;
import java.io.StringWriter;

public class DebugUtils {

    public static String stackTrace(Exception e){
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        return sw.toString();
    }

    public static void printStackTrace(){
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        StringBuilder sb = new StringBuilder();

        // 跳过前几个无关的栈帧
        for (int i = 0; i < stackTrace.length && i < Math.min(stackTrace.length, 45); i++) { // 限制栈深度
            StackTraceElement element = stackTrace[i];
            sb.append("\t").append(element).append("\n");
        }

        System.out.println(sb);

    }

}

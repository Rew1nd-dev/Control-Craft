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

}

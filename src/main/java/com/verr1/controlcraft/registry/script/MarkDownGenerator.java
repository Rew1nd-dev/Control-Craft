package com.verr1.controlcraft.registry.script;

import com.verr1.controlcraft.content.cctweaked.peripheral.*;
import dan200.computercraft.api.lua.LuaFunction;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.List;

public class MarkDownGenerator {


    static List<Class<?>> ALL_CLASSES = List.of(
            CameraPeripheral.class,
            CompactFlapPeripheral.class,
            DynamicMotorPeripheral.class,
            FlapBearingPeripheral.class,
            JetPeripheral.class,
            KinematicMotorPeripheral.class,
            PropellerControllerPeripheral.class,
            SliderPeripheral.class,
            SpatialAnchorPeripheral.class,
            SpinalyzerPeripheral.class,
            TransmitterPeripheral.class
    );

    public static void main(String[] args) {
        String outputDir = System.getProperty("user.dir") + "\\doc\\cc-peripherals\\template";

        try {
            for (Class<?> clazz : ALL_CLASSES) {
                String markdown = generate(clazz);
                saveMarkdown(clazz.getSimpleName() + ".md", markdown, outputDir);
            }
            System.out.println("Markdown templates generated successfully in " + outputDir);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }



    private static String generate(Class<?> clazz) {
        StringBuilder markdown = new StringBuilder();
        markdown.append("# ").append(clazz.getSimpleName()).append(" Methods\n\n");
        markdown.append("This document describes the Lua methods available in the `").append(clazz.getSimpleName()).append("` peripheral.\n\n");

        for (Method method : clazz.getDeclaredMethods()) {
            if (method.isAnnotationPresent(LuaFunction.class)) {
                // 构建方法名和参数列表
                StringBuilder methodHeader = new StringBuilder("`").append(method.getName());
                Parameter[] params = method.getParameters();
                if (params.length > 0) {
                    methodHeader.append("(");
                    for (int i = 0; i < params.length; i++) {
                        methodHeader.append(params[i].getName());
                        if (i < params.length - 1) {
                            methodHeader.append(", ");
                        }
                    }
                    methodHeader.append(")");
                }
                methodHeader.append("`");
                markdown.append("### ").append(methodHeader).append("\n");

                // 参数列表
                markdown.append("- **参数**：\n");
                if (params.length == 0) {
                    markdown.append("  - 无\n");
                } else {
                    for (Parameter param : params) {
                        markdown.append("  - `").append(param.getName()).append("` (`")
                                .append(param.getType().getSimpleName()).append("`): \n");
                    }
                }

                // 返回值和后续部分
                markdown.append("- **返回值**：`").append(method.getReturnType().getSimpleName()).append("` - \n");
                markdown.append("- **描述**：\n");
                markdown.append("- **示例**：\n");
                markdown.append("  ```lua\n\n  ```\n\n");
            }
        }
        return markdown.toString();
    }

    private static void saveMarkdown(String fileName, String content, String outputDir) throws IOException {
        File dir = new File(outputDir);
        if (!dir.exists()) {
            dir.mkdirs();
        }
        File file = new File(dir, fileName);
        try (PrintWriter writer = new PrintWriter(file)) {
            writer.println(content);
        }
    }



}

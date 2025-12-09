package dsl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

class Interpreter {
    private final Map<String, Double> variables = new HashMap<>();
    private final Map<String, FunctionDef> userFunctions = new HashMap<>();
    private final Map<String, Function<List<Double>, Double>> builtInFunctions = new HashMap<>();

    Interpreter() {
        // 内置函数
        builtInFunctions.put("sin", args -> Math.sin(args.get(0)));
        builtInFunctions.put("cos", args -> Math.cos(args.get(0)));
        builtInFunctions.put("min", args -> Math.min(args.get(0), args.get(1)));
        builtInFunctions.put("max", args -> Math.max(args.get(0), args.get(1)));
        // 添加print作为示例（输出到控制台）
        builtInFunctions.put("print", args -> {
            System.out.println(args.get(0));
            return 0.0;
        });
    }

    // 扩展函数：添加新内置函数
    void addBuiltInFunction(String name, Function<List<Double>, Double> impl) {
        builtInFunctions.put(name, impl);
    }

    void interpret(ProgramNode program) {
        program.execute(variables, builtInFunctions, userFunctions);
    }

    Map<String, Double> getVariables() {
        return variables;
    }
}
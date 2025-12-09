package dsl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

abstract class ASTNode {
    abstract Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions);
}

// 数字节点
class NumberNode extends ASTNode {
    double value;
    NumberNode(double value) { this.value = value; }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        return value;
    }
}

// 变量节点
class VarNode extends ASTNode {
    String name;
    VarNode(String name) { this.name = name; }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        return variables.getOrDefault(name, 0.0);
    }
}

// 二元运算节点
class BinOpNode extends ASTNode {
    ASTNode left, right;
    TokenType op;
    BinOpNode(ASTNode left, TokenType op, ASTNode right) {
        this.left = left;
        this.op = op;
        this.right = right;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        double l = (Double) left.execute(variables, builtInFunctions, userFunctions);
        double r = (Double) right.execute(variables, builtInFunctions, userFunctions);
        switch (op) {
            case PLUS: return l + r;
            case MINUS: return l - r;
            case MUL: return l * r;
            case DIV: return l / r;
            case GT: return l > r ? 1.0 : 0.0;
            case LT: return l < r ? 1.0 : 0.0;
            case EQ: return l == r ? 1.0 : 0.0;
            default: throw new RuntimeException("Unknown op: " + op);
        }
    }
}

// 函数调用节点
class FuncCallNode extends ASTNode {
    String name;
    List<ASTNode> args;
    FuncCallNode(String name, List<ASTNode> args) {
        this.name = name;
        this.args = args;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        List<Double> argValues = args.stream().map(a -> (Double) a.execute(variables, builtInFunctions, userFunctions)).toList();
        if (builtInFunctions.containsKey(name)) {
            return builtInFunctions.get(name).apply(argValues);
        } else if (userFunctions.containsKey(name)) {
            return userFunctions.get(name).execute(argValues, variables, builtInFunctions, userFunctions);
        } else {
            throw new RuntimeException("Unknown function: " + name);
        }
    }
}

// 赋值语句
class AssignNode extends ASTNode {
    String varName;
    ASTNode expr;
    AssignNode(String varName, ASTNode expr) {
        this.varName = varName;
        this.expr = expr;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        double value = (Double) expr.execute(variables, builtInFunctions, userFunctions);
        variables.put(varName, value);
        return null;
    }
}

// 变量声明（double x = ...）
class VarDeclNode extends ASTNode {
    String name;
    ASTNode init;
    VarDeclNode(String name, ASTNode init) {
        this.name = name;
        this.init = init;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        if (init != null) {
            double value = (Double) init.execute(variables, builtInFunctions, userFunctions);
            variables.put(name, value);
        } else {
            variables.put(name, 0.0);
        }
        return null;
    }
}

// 多变量声明：double x, y = func();
class MultiVarDeclNode extends ASTNode {
    List<String> names;
    ASTNode init;  // 必须是FuncCallNode或其他返回List的
    MultiVarDeclNode(List<String> names, ASTNode init) {
        this.names = names;
        this.init = init;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        Object result = init.execute(variables, builtInFunctions, userFunctions);
        List<Double> values = Util.toList(result);
        if (values.size() != names.size()) {
            throw new RuntimeException("Return count mismatch: expected " + names.size() + ", got " + values.size());
        }
        for (int i = 0; i < names.size(); i++) {
            variables.put(names.get(i), values.get(i));
        }
        return null;
    }
}

// 多赋值：x, y = func();  (假设变量已存在)
class MultiAssignNode extends ASTNode {
    List<String> names;
    ASTNode expr;
    MultiAssignNode(List<String> names, ASTNode expr) {
        this.names = names;
        this.expr = expr;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        Object result = expr.execute(variables, builtInFunctions, userFunctions);
        List<Double> values = Util.toList(result);
        if (values.size() != names.size()) {
            throw new RuntimeException("Return count mismatch: expected " + names.size() + ", got " + values.size());
        }
        for (int i = 0; i < names.size(); i++) {
            if (!variables.containsKey(names.get(i))) {
                throw new RuntimeException("Variable " + names.get(i) + " not declared");
            }
            variables.put(names.get(i), values.get(i));
        }
        return null;
    }
}



// If 语句
class IfNode extends ASTNode {
    ASTNode condition;
    List<ASTNode> thenBody;
    List<ASTNode> elseBody;
    IfNode(ASTNode condition, List<ASTNode> thenBody, List<ASTNode> elseBody) {
        this.condition = condition;
        this.thenBody = thenBody;
        this.elseBody = elseBody;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        double cond = (Double) condition.execute(variables, builtInFunctions, userFunctions);
        if (cond != 0.0) {
            for (ASTNode stmt : thenBody) stmt.execute(variables, builtInFunctions, userFunctions);
        } else if (elseBody != null) {
            for (ASTNode stmt : elseBody) stmt.execute(variables, builtInFunctions, userFunctions);
        }
        return null;
    }
}

// While 语句
class WhileNode extends ASTNode {
    ASTNode condition;
    List<ASTNode> body;
    WhileNode(ASTNode condition, List<ASTNode> body) {
        this.condition = condition;
        this.body = body;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        while ((Double) condition.execute(variables, builtInFunctions, userFunctions) != 0.0) {
            for (ASTNode stmt : body) stmt.execute(variables, builtInFunctions, userFunctions);
        }
        return null;
    }
}

// 函数定义
class FunctionDef {
    List<String> params;
    List<ASTNode> body;

    Object execute(List<Double> args, Map<String, Double> globalVars, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        Map<String, Double> localVars = new HashMap<>(globalVars);
        for (int i = 0; i < params.size(); i++) {
            localVars.put(params.get(i), args.get(i));
        }
        for (ASTNode stmt : body) {
            Object result = stmt.execute(localVars, builtInFunctions, userFunctions);
            if (result != null) {  // 如果return
                return result;
            }
        }
        return 0.0;  // 默认单值
    }
}

// 函数定义节点
class FuncDefNode extends ASTNode {
    String name;
    List<String> params;
    List<ASTNode> body;
    FuncDefNode(String name, List<String> params, List<ASTNode> body) {
        this.name = name;
        this.params = params;
        this.body = body;
    }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        FunctionDef def = new FunctionDef();
        def.params = params;
        def.body = body;  // Use the full body; the return is handled in the execute loop
        if(userFunctions.containsKey(name)){
            throw new RuntimeException("Function " + name + " already defined");
        }
        userFunctions.put(name, def);
        return null;
    }
}

class ReturnNode extends ASTNode {
    ASTNode expr;
    ReturnNode(ASTNode expr) { this.expr = expr; }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        return expr.execute(variables, builtInFunctions, userFunctions);
    }
}

// 多返回
class MultiReturnNode extends ASTNode {
    List<ASTNode> exprs;
    MultiReturnNode(List<ASTNode> exprs) { this.exprs = exprs; }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        return exprs.stream().map(e -> (Double) e.execute(variables, builtInFunctions, userFunctions)).toList();
    }
}



// 程序节点（语句列表）
class ProgramNode extends ASTNode {
    List<ASTNode> statements;
    ProgramNode(List<ASTNode> statements) { this.statements = statements; }
    @Override
    Object execute(Map<String, Double> variables, Map<String, Function<List<Double>, Double>> builtInFunctions, Map<String, FunctionDef> userFunctions) {
        for (ASTNode stmt : statements) {
            stmt.execute(variables, builtInFunctions, userFunctions);
        }
        return null;
    }
}


class Util {
    public static List<Double> toList(Object obj) {
        if (obj instanceof Double d) {
            return List.of(d);
        } else if (obj instanceof List<?> list) {
            return list.stream().map(v -> (Double) v).toList();
        }
        throw new RuntimeException("Invalid return type");
    }
}

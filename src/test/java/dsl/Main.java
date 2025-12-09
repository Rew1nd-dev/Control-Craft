package dsl;

public class Main {
    public static void main(String[] args) {
        String script = """
            def pair(a, b) {
                return a, b;
            }
            
            
            double x, y = pair(1.0, 2.0);
            print(x);
            print(y);
            
            """;

        Lexer lexer = new Lexer(script);
        Parser parser = new Parser(lexer.getTokens());
        ProgramNode ast = parser.parse();

        Interpreter interpreter = new Interpreter();
        // 示例扩展函数
        interpreter.addBuiltInFunction("pow", _args -> Math.pow(_args.get(0), _args.get(1)));

        interpreter.interpret(ast);

        System.out.println("Variables: " + interpreter.getVariables());
    }
}
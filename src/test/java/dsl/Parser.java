package dsl;

import java.util.ArrayList;
import java.util.List;

import java.util.ArrayList;
import java.util.List;

class Parser {
    private List<Token> tokens;
    private int pos = 0;

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    ProgramNode parse() {
        List<ASTNode> statements = new ArrayList<>();
        while (!isEOF()) {
            statements.add(parseStatement());
        }
        return new ProgramNode(statements);
    }

    private ASTNode parseStatement() {
        Token current = peek();
        if (current.type == TokenType.DOUBLE) {
            consume(TokenType.DOUBLE);
            List<String> names = new ArrayList<>();
            names.add(consume(TokenType.IDENTIFIER).value);
            while (peek().type == TokenType.COMMA) {
                consume(TokenType.COMMA);
                names.add(consume(TokenType.IDENTIFIER).value);
            }
            if (peek().type == TokenType.ASSIGN) {
                consume(TokenType.ASSIGN);
                ASTNode init = parseExpr();
                consume(TokenType.SEMI);
                if (names.size() == 1) {
                    return new VarDeclNode(names.get(0), init);
                } else {
                    return new MultiVarDeclNode(names, init);
                }
            } else {
                // 无初始化：double x, y;
                consume(TokenType.SEMI);
                if (names.size() == 1) {
                    return new VarDeclNode(names.get(0), null);
                } else {
                    // 多声明无初始化，默认为0
                    for (String name : names) {
                        if (!name.equals("_")) {  // 忽略_，但声明无init时无意义
                            // 可以添加默认0，但为简单，跳过_
                        }
                    }
                    return new ProgramNode(new ArrayList<>());  // 占位，实际可忽略
                }
            }
        } else if (current.type == TokenType.IDENTIFIER) {
            List<String> names = new ArrayList<>();
            names.add(consume(TokenType.IDENTIFIER).value);
            while (peek().type == TokenType.COMMA) {
                consume(TokenType.COMMA);
                names.add(consume(TokenType.IDENTIFIER).value);
            }
            if (peek().type == TokenType.ASSIGN) {
                consume(TokenType.ASSIGN);
                ASTNode expr = parseExpr();
                consume(TokenType.SEMI);
                if (names.size() == 1) {
                    return new AssignNode(names.get(0), expr);
                } else {
                    return new MultiAssignNode(names, expr);
                }
            } else if (peek().type == TokenType.LPAREN) {
                ASTNode call = parseFactor();  // 函数调用在 factor 中处理
                consume(TokenType.SEMI);
                return call;
            } else {
                consume(TokenType.SEMI);  // 空语句？
                return new ProgramNode(new ArrayList<>());
            }
        } else if (current.type == TokenType.IF) {
            consume(TokenType.IF);
            consume(TokenType.LPAREN);
            ASTNode cond = parseExpr();
            consume(TokenType.RPAREN);
            consume(TokenType.LBRACE);
            List<ASTNode> thenBody = parseBody();
            consume(TokenType.RBRACE);
            List<ASTNode> elseBody = null;
            if (peek().type == TokenType.ELSE) {
                consume(TokenType.ELSE);
                consume(TokenType.LBRACE);
                elseBody = parseBody();
                consume(TokenType.RBRACE);
            }
            return new IfNode(cond, thenBody, elseBody);
        } else if (current.type == TokenType.WHILE) {
            consume(TokenType.WHILE);
            consume(TokenType.LPAREN);
            ASTNode cond = parseExpr();
            consume(TokenType.RPAREN);
            consume(TokenType.LBRACE);
            List<ASTNode> body = parseBody();
            consume(TokenType.RBRACE);
            return new WhileNode(cond, body);
        } else if (current.type == TokenType.DEF) {
            consume(TokenType.DEF);
            String name = consume(TokenType.IDENTIFIER).value;
            consume(TokenType.LPAREN);
            List<String> params = new ArrayList<>();
            while (peek().type != TokenType.RPAREN) {
                params.add(consume(TokenType.IDENTIFIER).value);
                if (peek().type == TokenType.COMMA) consume(TokenType.COMMA);
            }
            consume(TokenType.RPAREN);
            consume(TokenType.LBRACE);
            List<ASTNode> body = parseBody();
            consume(TokenType.RBRACE);
            return new FuncDefNode(name, params, body);
        } else if (current.type == TokenType.RETURN) {
            consume(TokenType.RETURN);
            List<ASTNode> exprs = new ArrayList<>();
            exprs.add(parseExpr());
            while (peek().type == TokenType.COMMA) {
                consume(TokenType.COMMA);
                exprs.add(parseExpr());
            }
            consume(TokenType.SEMI);
            if (exprs.size() == 1) {
                return new ReturnNode(exprs.get(0));
            } else {
                return new MultiReturnNode(exprs);
            }
        }
        throw new RuntimeException("Unexpected statement: " + current);
    }

    private List<ASTNode> parseBody() {
        List<ASTNode> body = new ArrayList<>();
        while (peek().type != TokenType.RBRACE && !isEOF()) {
            body.add(parseStatement());
        }
        return body;
    }

    // 表达式解析（支持运算符优先级）
    private ASTNode parseExpr() {
        return parseRelational();
    }

    private ASTNode parseRelational() {
        ASTNode left = parseAdditive();
        while (peek().type == TokenType.GT || peek().type == TokenType.LT || peek().type == TokenType.EQ) {
            TokenType op = consume().type;
            ASTNode right = parseAdditive();
            left = new BinOpNode(left, op, right);
        }
        return left;
    }

    private ASTNode parseAdditive() {
        ASTNode left = parseTerm();
        while (peek().type == TokenType.PLUS || peek().type == TokenType.MINUS) {
            TokenType op = consume().type;
            ASTNode right = parseTerm();
            left = new BinOpNode(left, op, right);
        }
        return left;
    }

    private ASTNode parseTerm() {
        ASTNode left = parseFactor();
        while (peek().type == TokenType.MUL || peek().type == TokenType.DIV) {
            TokenType op = consume().type;
            ASTNode right = parseFactor();
            left = new BinOpNode(left, op, right);
        }
        return left;
    }

    private ASTNode parseFactor() {
        Token current = peek();
        if (current.type == TokenType.NUMBER) {
            return new NumberNode(Double.parseDouble(consume().value));
        } else if (current.type == TokenType.IDENTIFIER) {
            String name = consume().value;
            if (peek().type == TokenType.LPAREN) {
                consume(TokenType.LPAREN);
                List<ASTNode> args = new ArrayList<>();
                while (peek().type != TokenType.RPAREN) {
                    args.add(parseExpr());
                    if (peek().type == TokenType.COMMA) consume(TokenType.COMMA);
                }
                consume(TokenType.RPAREN);
                return new FuncCallNode(name, args);
            } else {
                return new VarNode(name);
            }
        } else if (current.type == TokenType.LPAREN) {
            consume(TokenType.LPAREN);
            ASTNode expr = parseExpr();
            consume(TokenType.RPAREN);
            return expr;
        }
        throw new RuntimeException("Unexpected factor: " + current);
    }

    private Token consume() {
        return tokens.get(pos++);
    }

    private Token consume(TokenType type) {
        Token t = peek();
        if (t.type != type) throw new RuntimeException("Expected " + type + ", got " + t.type);
        pos++;
        return t;
    }

    private Token peek() {
        if (pos >= tokens.size()) return new Token(TokenType.EOF, null);
        return tokens.get(pos);
    }

    private boolean isEOF() {
        return peek().type == TokenType.EOF;
    }
}
package dsl;

import java.util.List;

enum TokenType {
    // 关键词
    DOUBLE, IF, ELSE, WHILE, DEF, RETURN,
    // 运算符
    PLUS, MINUS, MUL, DIV, GT, LT, EQ, ASSIGN,
    // 其他
    LPAREN, RPAREN, LBRACE, RBRACE, SEMI, COMMA,
    IDENTIFIER, NUMBER, EOF
}

class Token {
    TokenType type;
    String value;  // 对于数字或标识符

    Token(TokenType type, String value) {
        this.type = type;
        this.value = value;
    }

    @Override
    public String toString() {
        return type + (value != null ? ":" + value : "");
    }
}

package dsl;

import java.util.ArrayList;
import java.util.List;

class Lexer {
    private String input;
    private int pos = 0;
    private List<Token> tokens = new ArrayList<>();

    Lexer(String input) {
        this.input = input;
        tokenize();
    }

    List<Token> getTokens() {
        return tokens;
    }

    private void tokenize() {
        while (pos < input.length()) {
            char ch = input.charAt(pos);
            if (Character.isWhitespace(ch)) {
                pos++;
                continue;
            }
            if (Character.isDigit(ch) || ch == '.') {
                tokens.add(new Token(TokenType.NUMBER, readNumber()));
                continue;
            }
            if (Character.isLetter(ch)) {
                String word = readWord();
                switch (word) {
                    case "double": tokens.add(new Token(TokenType.DOUBLE, null)); break;
                    case "if": tokens.add(new Token(TokenType.IF, null)); break;
                    case "else": tokens.add(new Token(TokenType.ELSE, null)); break;
                    case "while": tokens.add(new Token(TokenType.WHILE, null)); break;
                    case "def": tokens.add(new Token(TokenType.DEF, null)); break;
                    case "return": tokens.add(new Token(TokenType.RETURN, null)); break;
                    default: tokens.add(new Token(TokenType.IDENTIFIER, word)); break;
                }
                continue;
            }
            switch (ch) {
                case '+': tokens.add(new Token(TokenType.PLUS, null)); break;
                case '-': tokens.add(new Token(TokenType.MINUS, null)); break;
                case '*': tokens.add(new Token(TokenType.MUL, null)); break;
                case '/': tokens.add(new Token(TokenType.DIV, null)); break;
                case '>': tokens.add(new Token(TokenType.GT, null)); break;
                case '<': tokens.add(new Token(TokenType.LT, null)); break;
                case '=': 
                    if (peek() == '=') {
                        pos++;
                        tokens.add(new Token(TokenType.EQ, null));
                    } else {
                        tokens.add(new Token(TokenType.ASSIGN, null));
                    }
                    break;
                case '(': tokens.add(new Token(TokenType.LPAREN, null)); break;
                case ')': tokens.add(new Token(TokenType.RPAREN, null)); break;
                case '{': tokens.add(new Token(TokenType.LBRACE, null)); break;
                case '}': tokens.add(new Token(TokenType.RBRACE, null)); break;
                case ';': tokens.add(new Token(TokenType.SEMI, null)); break;
                case ',': tokens.add(new Token(TokenType.COMMA, null)); break;
                default: throw new RuntimeException("Unexpected char: " + ch);
            }
            pos++;
        }
        tokens.add(new Token(TokenType.EOF, null));
    }

    private String readNumber() {
        int start = pos;
        while (pos < input.length() && (Character.isDigit(input.charAt(pos)) || input.charAt(pos) == '.')) pos++;
        return input.substring(start, pos);
    }

    private String readWord() {
        int start = pos;
        while (pos < input.length() && Character.isLetterOrDigit(input.charAt(pos))) pos++;
        return input.substring(start, pos);
    }

    private char peek() {
        if (pos + 1 < input.length()) return input.charAt(pos + 1);
        return '\0';
    }
}
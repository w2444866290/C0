package miniplc0java.tokenizer;

import miniplc0java.error.TokenizeError;
import miniplc0java.error.ErrorCode;
import org.checkerframework.checker.units.qual.Speed;

import java.util.ArrayList;
import java.util.List;

public class Tokenizer {

    private StringIter it;

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    // 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了
    /**
     * 获取下一个 Token
     * 
     * @return
     * @throws TokenizeError 如果解析有异常则抛出
     */
    public Token nextToken() throws TokenizeError {
        it.readAll();

        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();

        if (Character.isDigit(peek)) {
            return lexUInt();
        } else if (peek == '"') {
            return lexStringLiteral();
        }  else if (Character.isLetter(peek) || peek == '_') {
            return lexIdentOrKeyword();
        } else {
            return lexOperatorOrUnknown();
        }
    }

    private Token lexUInt() throws TokenizeError {
        char peek = it.peekChar();
        StringBuilder buf = new StringBuilder();

        // 直到查看下一个字符不是数字为止:
        // -- 前进一个字符，并存储这个字符
        while (Character.isDigit(peek)) {
            buf.append(it.nextChar());
            peek = it.peekChar();
        }

        // 解析存储的字符串为无符号整数
        // 解析成功则返回无符号整数类型的token，否则返回编译错误
        int uint = Integer.parseInt(buf.toString());
        if (!Double.isNaN(uint)) {
            return new Token(TokenType.UINT, uint, it.previousPos(), it.currentPos());
        }
        else {
            // todo: is it right?
            throw new TokenizeError(ErrorCode.ConstantNeedValue, it.currentPos());
        }
    }

    private Token lexStringLiteral() throws TokenizeError {
        StringBuilder buf = new StringBuilder();
        it.nextChar();
        char peek = it.peekChar();

        // 直到查看下一个字符不是数字或字母为止:
        // -- 前进一个字符，并存储这个字符
        while (true) {
            // end
            if (peek == '"') {
                it.nextChar();
                peek = it.peekChar();
                break;
            }
            if (it.isEOF())
                throw new TokenizeError(ErrorCode.InvalidInput, it.currentPos());

            // escape_sequence
            if (peek == '\\') {
                it.nextChar();
                peek = it.peekChar();
                switch (peek) {
                    case '\\': buf.append('\\');break;
                    case '"': buf.append('"');break;
                    case '\'': buf.append('\'');break;
                    case 'n': buf.append('\n');break;
                    case 't': buf.append('\t');break;
                    case 'r': buf.append('\r');break;
                    default:
                        buf.append('\\');
                }
                it.nextChar();
                peek = it.peekChar();
            }
            // string_regular_char
            else {
                buf.append(it.nextChar());
                peek = it.peekChar();
            }
        }

        // 返回字符串类型的token
        return new Token(TokenType.STRING, buf.toString(), it.previousPos(), it.currentPos());
    }

    private Token lexIdentOrKeyword() throws TokenizeError {
        StringBuilder buf = new StringBuilder();
        buf.append(it.nextChar());
        char peek = it.peekChar();

        // 直到查看下一个字符不是数字或字母为止:
        // -- 前进一个字符，并存储这个字符
        while (Character.isLetterOrDigit(peek) || peek == '_') {
            buf.append(it.nextChar());
            peek = it.peekChar();
        }

        // 尝试将存储的字符串解释为关键字
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        String token = buf.toString();
        switch (token) {
            case "fn":
                return  new Token(TokenType.FN_KW, "fn", it.previousPos(), it.currentPos());

            case "let":
                return  new Token(TokenType.LET_KW, "let", it.previousPos(), it.currentPos());

            case "const":
                return  new Token(TokenType.CONST_KW, "const", it.previousPos(), it.currentPos());

            case "as":
                return  new Token(TokenType.AS_KW, "as", it.previousPos(), it.currentPos());

            case "while":
                return  new Token(TokenType.WHILE_KW, "while", it.previousPos(), it.currentPos());

            case "if":
                return  new Token(TokenType.IF_KW, "if", it.previousPos(), it.currentPos());

            case "else":
                return  new Token(TokenType.ELSE_KW, "else", it.previousPos(), it.currentPos());

            case "return":
                return  new Token(TokenType.RETURN_KW, "return", it.previousPos(), it.currentPos());

            default:
                return new Token(TokenType.IDENT, token, it.previousPos(), it.currentPos());
        }
    }

    private Token lexOperatorOrUnknown() throws TokenizeError {
        switch (it.nextChar()) {
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());

            case '-':
                if (it.peekChar() == '>') {
                    it.nextChar();
                    return new Token(TokenType.ARROW, "->", it.previousPos(), it.currentPos());
                }
                else return new Token(TokenType.MINUS, '-', it.previousPos(), it.currentPos());

            case '*':
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());

            case '/':
                // 检查是否为注释
                if (it.peekChar() == '/') {
                    var peek = it.nextChar();
                    do {
                        peek = it.nextChar();
                    } while (peek != '\n');
                    return new Token(TokenType.NONE, "", it.previousPos(), it.currentPos());
                }
                else return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());

            case '=':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.EQ, "==", it.previousPos(), it.currentPos());
                }
                else return new Token(TokenType.ASSIGN, '=', it.previousPos(), it.currentPos());

            case '!':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.NEQ, "!=", it.previousPos(), it.currentPos());
                }

            case '<':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.LE, "<=", it.previousPos(), it.currentPos());
                }
                else return new Token(TokenType.LT, '<', it.previousPos(), it.currentPos());

            case '>':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.GE, ">=", it.previousPos(), it.currentPos());
                }
                else return new Token(TokenType.GT, '>', it.previousPos(), it.currentPos());

            case '(':
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());

            case ')':
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());

            case '{':
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());

            case '}':
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());

            case ',':
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());

            case ':':
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());

            case ';':
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());

            default:
                // 不认识这个输入，摸了
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }

    public String Run() {
        var tokens = new ArrayList<Token>();
        var tokenbuilder = new StringBuilder();
        try {
            while (true) {
                var token = this.nextToken();
                if (token.getTokenType().equals(TokenType.NONE))
                    continue;
                if (token.getTokenType().equals(TokenType.EOF)) {
                    tokenbuilder.append('/');
                    break;
                }
                tokens.add(token);
                tokenbuilder.append(token.getTokenType().toString());
                if (token.getTokenType() == TokenType.IDENT ||
                        token.getTokenType() == TokenType.UINT || token.getTokenType() == TokenType.STRING) {
                    tokenbuilder.append(token.getValue().toString());
                }
                tokenbuilder.append('/');
                System.out.println(token.getTokenType().toString() + " " + token.getValue().toString());
            }
            return tokenbuilder.toString();
        } catch (Exception e) {
            // 遇到错误不输出，直接退出
            System.err.println(e);
            System.exit(0);
            return null;
        }
    }
}

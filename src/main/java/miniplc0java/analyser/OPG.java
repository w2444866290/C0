package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.CompileError;
import miniplc0java.error.ErrorCode;
import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.TokenType;
import miniplc0java.tokenizer.Tokenizer;
import miniplc0java.util.Pos;

import java.util.Stack;

public class OPG {
    // 优先关系矩阵
    protected static final int[][] OPGTable =  {
            // 1 is =, 2 is <, 3 is >, 0 is error
            // '+,-' '*,/' i n ( ) '>,<,>=,<=,==,!=' as #
            {     3,    2, 2,2,2,3,               3,  2,3}, // 0: + -
            {     3,    3, 2,2,2,3,               3,  2,3}, // 1: * /
            {     3,    3, 0,0,0,3,               3,  3,3}, // 2: i
            {     3,    3, 2,2,2,3,               3,  3,3}, // 3: n
            {     2,    2, 2,2,2,1,               2,  2,0}, // 4: (
            {     3,    3, 0,0,0,3,               3,  3,3}, // 5: )
            {     2,    2, 2,2,2,3,               3,  2,3}, // 6: < > <= >= == !=
            {     3,    3, 1,0,0,3,               3,  0,3}, // 7: as
            {     2,    2, 2,2,2,0,               2,  2,3}, // 8: #
    };

    public OPG() {}

    public int[][] getOPGTable() {
        return OPGTable;
    }

    public static boolean isOperator(String s) {
        switch (s) {
            case "i":
            case "as":
            case "+":
            case "-":
            case "n":
            case "*":
            case "/":
            case "==":
            case "!=":
            case "<":
            case ">":
            case "<=":
            case ">=":
            case "(":
            case ")":
            case "#":
                return true;
            default:
                return false;
        }
    }

    public static int compare(Token a, Token b) {
        int ia = searchOPGTable(getSymbol(a.getTokenType()));
        int ib = searchOPGTable(getSymbol(b.getTokenType()));

        if (ia != -1 && ib != -1)
            return OPGTable[ia][ib];
        else
            return -1;
    }

    public static int searchOPGTable(String entry) {
        switch (entry) {
            case "+":
            case "-":
                return 0;
            case "*":
            case "/":
                return 1;
            case "i":
                return 2;
            case "n":
                return 3;
            case "(":
                return 4;
            case ")":
                return 5;
            case ">":
            case "<":
            case ">=":
            case "<=":
            case "==":
            case "!=":
                return 6;
            case "as":
                return 7;
            case "#":
                return 8;
            default:
                return -1;
        }
    }

    public static Token getOpIn(Stack<Token> symbolStack) {
        Token res = null;

        for (Token t:
                symbolStack) {
            if (isOperator(getSymbol(t.getTokenType())))
                res = t;
        }

        return res;
    }

    public static String getSymbol(TokenType tt) {
        switch (tt) {
            case UINT:
            case STRING:
            case DOUBLE:
            case IDENT:
                return "i";
            case AS_KW:
                return "as";
            case PLUS:
                return "+";
            case MINUS:
                return "-";
            case NEGATE:
                return "n";
            case MUL:
                return "*";
            case DIV:
                return "/";
            case ASSIGN:
                return "=";
            case EQ:
                return "==";
            case NEQ:
                return "!=";
            case LT:
                return "<";
            case GT:
                return ">";
            case LE:
                return "<=";
            case GE:
                return ">=";
            case L_PAREN:
                return "(";
            case R_PAREN:
                return ")";
            case END:
                return "#";
            case NONE:
                return "N";
            default:
                return null;
        }
    }
}

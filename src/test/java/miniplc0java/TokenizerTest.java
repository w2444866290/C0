package miniplc0java;

import miniplc0java.analyser.Analyser;
import miniplc0java.tokenizer.StringIter;
import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.Tokenizer;
import org.junit.Test;

import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;

import static org.junit.Assert.*;

public class TokenizerTest {
    public String RunTokenizer(StringIter it) {
        var tokenizer = new Tokenizer(it);
        var outString = new String();

        outString = tokenizer.Run();

        return outString;
    }

    @Test
    public void TestlexUInt() {
        Scanner scanner;
        String input = new String("1 0 010");
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("UINT1/UINT0/UINT10//", outString);
    }

    @Test
    public void TestlexDouble() {
        Scanner scanner;
        String input = new String("0.01 01.1 1.0 2.1E2 2.1E+2 2.1E-2 2.1e-2");
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("DOUBLE4576918229304087675/" +
                "DOUBLE4607632778762754458/" +
                "DOUBLE4607182418800017408/" +
                "DOUBLE4641592734702895104/DOUBLE4641592734702895104/" +
                "DOUBLE4581710059307609883/DOUBLE4581710059307609883//", outString);
    }

    @Test
    public void TestlexStringLiteral() {
        Scanner scanner;
        String input = new String("\"\\\\\\n\\t\\r\" \"hello!\"");
        System.out.println(input);
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("STRING\"\\\\\\n\\t\\r\"/STRING\"hello!\"//", outString);
    }

    @Test
    public void TestlexCharLiteral() {
        Scanner scanner;
        String input = new String("\'t\'  \'\n\'  \'\'\'");
        System.out.println(input);
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("CHAR116/CHAR10/CHAR39//", outString);
    }

    @Test
    public void TestlexIdentOrKeyword() {
        Scanner scanner;
        String input = new String("\t\nfn let const as while if else return break continue _abc ident");
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("FN_KW/LET_KW/CONST_KW/AS_KW/WHILE_KW/IF_KW/ELSE_KW/RETURN_KW/BREAK_KW/CONTINUE_KW/IDENT_abc/IDENTident//", outString);
    }

    @Test
    public void TestlexOperatorOrUnknown() {
        Scanner scanner;
        String input = new String(", : ; -> + - * / = == != < > <= >= ( ) { }");
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("COMMA/COLON/SEMICOLON/ARROW/PLUS/MINUS/MUL/DIV/ASSIGN/EQ/NEQ/LT/GT/LE/GE/L_PAREN/R_PAREN/L_BRACE/R_BRACE//", outString);
    }

    @Test
    public void TestComment() {
        Scanner scanner;
        String input = new String("abc " +
                "\n" +
                "//this is a comment!\n");
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);

        var outString = RunTokenizer(iter);

        assertEquals("IDENTabc//", outString);
    }

    @Test
    public void TestLEX() {
        Scanner scanner;
        try {
            FileInputStream input = new FileInputStream("analysertest.txt");
            scanner = new Scanner(input);
            var iter = new StringIter(scanner);
            var tokenizer = new Tokenizer(iter);
            tokenizer.Run();
        }
        catch (Exception e) {
            System.err.println(e);
            System.exit(-1);
        }
    }
}

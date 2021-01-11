package miniplc0java;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import miniplc0java.analyser.Analyser;
import miniplc0java.error.CompileError;
import miniplc0java.instruction.Instruction;
import miniplc0java.tokenizer.StringIter;
import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.TokenType;
import miniplc0java.tokenizer.Tokenizer;

import net.sourceforge.argparse4j.*;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentAction;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.Namespace;

public class App {
    public static void main(String[] args) throws CompileError {
        var argparse = buildArgparse();
        Namespace result;
        try {
            result = argparse.parseArgs(args);
        } catch (ArgumentParserException e1) {
            argparse.handleError(e1);
            return;
        }

        var inputFileName = result.getString("input");
        var outputFileName = result.getString("output");

        InputStream input;
        if (inputFileName.equals("-")) {
            input = System.in;
        } else {
            try {
                input = new FileInputStream(inputFileName);
            } catch (FileNotFoundException e) {
                System.err.println("Cannot find input file.");
                e.printStackTrace();
                System.exit(2);
                return;
            }
        }

        PrintStream output;
        if (outputFileName.equals("-")) {
            output = System.out;
        } else {
            try {
                output = new PrintStream(new FileOutputStream(outputFileName));
            } catch (FileNotFoundException e) {
                System.err.println("Cannot open output file.");
                e.printStackTrace();
                System.exit(2);
                return;
            }
        }

        Scanner scanner;
        scanner = new Scanner(input);
        var iter = new StringIter(scanner);
        var tokenizer = tokenize(iter);

        if (result.getBoolean("tokenize")) {
            // tokenize
            var tokens = new ArrayList<Token>();
            try {
                while (true) {
                    var token = tokenizer.nextToken();
                    if (token.getTokenType().equals(TokenType.EOF)) {
                        break;
                    }
                    tokens.add(token);
                }
            } catch (Exception e) {
                // 遇到错误不输出，直接退出
                System.err.println(e);
                System.exit(-1);
                return;
            }
            for (Token token : tokens) {
                output.println(token.toString());
            }
        } else if (result.getBoolean("analyse")) {
            // analyze
            var analyzer = new Analyser(tokenizer);
            List<Object> commands;
            try {
                commands = analyzer.analyse();
            } catch (Exception e) {
                // 遇到错误不输出，直接退出
                System.err.println(e);
                System.exit(-1);
                return;
            }
            // magic
            output.write(Integer.parseInt("72", 16));
            output.write(Integer.parseInt("30", 16));
            output.write(Integer.parseInt("3b", 16));
            output.write(Integer.parseInt("3e", 16));
            // 版本号
            output.write(0);
            output.write(0);
            output.write(0);
            output.write(1);

            // 全局变量数
            String globCount =
                    hexaZeroFill(Integer.toHexString(Integer.parseInt(commands.get(0).toString())), 8);
            for (int i = 0; i < globCount.length(); i = i + 2) {
                StringBuilder num = new StringBuilder();
                num.append(globCount.charAt(i));
                num.append(globCount.charAt(i+1));
                output.write(Integer.parseInt(num.toString(), 16));
            }

            // 全局变量
            var ptr = 0;
            for (int i = 1; !commands.get(i).equals("#"); i = i + 3) {
                var is_const = Integer.parseInt(commands.get(i).toString());
                var valueCount = Integer.parseInt(commands.get(i+1).toString());
                var valueItems = commands.get(i+2);

                // is_const
                output.write(is_const);

                // value.count
                String count = hexaZeroFill(Integer.toHexString(valueCount), 8);
                for (int k = 0; k < count.length(); k = k + 2) {
                    StringBuilder num = new StringBuilder();
                    num.append(count.charAt(k));
                    num.append(count.charAt(k+1));
                    output.write(Integer.parseInt(num.toString(), 16));
                }

                // value.items
                // 如果是字符串 或 函数声明
                if (valueItems instanceof String) {
                    for (int k = 0; k < valueItems.toString().length(); k++)
                        output.write(((String) valueItems).charAt(k));
                }
                // 否则是全局变量， 赋值为0
                else {
                    output.write(0);
                    output.write(0);
                    output.write(0);
                    output.write(0);
                    output.write(0);
                    output.write(0);
                    output.write(0);
                    output.write(0);
                }
                ptr = i + 3;
            }

            // 函数个数
            String funcCount =
                    hexaZeroFill(Integer.toHexString(Integer.parseInt(commands.get(ptr+1).toString())), 8);
            for (int i = 0; i < funcCount.length(); i = i + 2) {
                StringBuilder num = new StringBuilder();
                num.append(funcCount.charAt(i));
                num.append(funcCount.charAt(i+1));
                output.write(Integer.parseInt(num.toString(), 16));
            }

            // 函数
            var bodyLength = 1;
            for (int i = ptr+2; i < commands.size(); i = i + 5 + bodyLength) {
                var name= Integer.parseInt(commands.get(i).toString());
                var ret_slots = Integer.parseInt(commands.get(i+1).toString());
                var param_slots = Integer.parseInt(commands.get(i+2).toString());
                var loc_slots = Integer.parseInt(commands.get(i+3).toString());
                var body_count = Integer.parseInt(commands.get(i+4).toString());
                bodyLength = body_count;

                // name
                String nameString = hexaZeroFill(Integer.toHexString(name), 8);
                for (int k = 0; k < nameString.length(); k = k + 2) {
                    StringBuilder num = new StringBuilder();
                    num.append(nameString.charAt(k));
                    num.append(nameString.charAt(k+1));
                    output.write(Integer.parseInt(num.toString(), 16));
                }

                // ret_slots
                String retSlots = hexaZeroFill(Integer.toHexString(ret_slots), 8);
                for (int k = 0; k < retSlots.length(); k = k + 2) {
                    StringBuilder num = new StringBuilder();
                    num.append(retSlots.charAt(k));
                    num.append(retSlots.charAt(k+1));
                    output.write(Integer.parseInt(num.toString(), 16));
                }

                // param_slots
                String paramSlots = hexaZeroFill(Integer.toHexString(param_slots), 8);
                for (int k = 0; k < paramSlots.length(); k = k + 2) {
                    StringBuilder num = new StringBuilder();
                    num.append(paramSlots.charAt(k));
                    num.append(paramSlots.charAt(k+1));
                    output.write(Integer.parseInt(num.toString(), 16));
                }

                // loc_slots
                String locSlots = hexaZeroFill(Integer.toHexString(loc_slots), 8);
                for (int k = 0; k < locSlots.length(); k = k + 2) {
                    StringBuilder num = new StringBuilder();
                    num.append(locSlots.charAt(k));
                    num.append(locSlots.charAt(k+1));
                    output.write(Integer.parseInt(num.toString(), 16));
                }

                // body_count
                String bodyCount = hexaZeroFill(Integer.toHexString(body_count), 8);
                for (int k = 0; k < bodyCount.length(); k = k + 2) {
                    StringBuilder num = new StringBuilder();
                    num.append(bodyCount.charAt(k));
                    num.append(bodyCount.charAt(k+1));
                    output.write(Integer.parseInt(num.toString(), 16));
                }

                for (int k = i+5; k <  i + 5 + body_count; k++) {
                    String ins = translateCommand((Instruction) commands.get(k));
                    for (int j = 0; j < ins.length(); j = j + 2) {
                        StringBuilder num = new StringBuilder();
                        num.append(ins.charAt(j));
                        num.append(ins.charAt(j+1));
                        output.write(Integer.parseInt(num.toString(),16));
                    }
                }
            }
        } else {
            System.err.println("Please specify either '--analyse' or '--tokenize'.");
            System.exit(3);
        }
    }

    private static ArgumentParser buildArgparse() {
        var builder = ArgumentParsers.newFor("miniplc0-java");
        var parser = builder.build();
        parser.addArgument("-t", "--tokenize").help("Tokenize the input").action(Arguments.storeTrue());
        parser.addArgument("-l", "--analyse").help("Analyze the input").action(Arguments.storeTrue());
        parser.addArgument("-o", "--output").help("Set the output file").required(true).dest("output")
                .action(Arguments.store());
        parser.addArgument("file").required(true).dest("input").action(Arguments.store()).help("Input file");
        return parser;
    }

    private static Tokenizer tokenize(StringIter iter) {
        var tokenizer = new Tokenizer(iter);
        return tokenizer;
    }

    private static String hexaZeroFill(String hexNum, int zeroNum) {
        int restLength = zeroNum - hexNum.length();
        StringBuilder GC = new StringBuilder();
        while (restLength-- > 0) {
            GC.append(0);
        }
        GC.append(hexNum);
        return GC.toString();
    }

    private static String translateCommand(Instruction ins) {
        String x = null;
        StringBuilder res = new StringBuilder();

        switch (ins.getOpt()) {
            case nop:
                return "00";
            case pop:
                return "02";
            case dup:
                return "04";
            case load8:
                return "10";
            case load16:
                return "11";
            case load32:
                return "12";
            case load64:
                return "13";
            case store8:
                return "14";
            case store16:
                return "15";
            case store32:
                return "16";
            case store64:
                return "17";
            case alloc:
                return "18";
            case free:
                return "19";
            case addi:
                return "20";
            case subi:
                return "21";
            case muli:
                return "22";
            case divi:
                return "23";
            case addf:
                return "24";
            case subf:
                return "25";
            case mulf:
                return "26";
            case divf:
                return "27";
            case divu:
                return "28";
            case shl:
                return "29";
            case shr:
                return "2a";
            case and:
                return "2b";
            case or:
                return "2c";
            case xor:
                return "2d";
            case not:
                return "2e";
            case cmpi:
                return "30";
            case cmpu:
                return "31";
            case cmpf:
                return "32";
            case negi:
                return "34";
            case negf:
                return "35";
            case itof:
                return "36";
            case ftoi:
                return "37";
            case shrl:
                return "38";
            case setlt:
                return "39";
            case setgt:
                return "3a";
            case ret:
                return "49";
            case scani:
                return "50";
            case scanc:
                return "51";
            case scanf:
                return "52";
            case printi:
                return "54";
            case printc:
                return "55";
            case printf:
                return "56";
            case prints:
                return "57";
            case println:
                return "58";
            case panic:
                return "fe";
            case push:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 16);
                res.append("01");
                res.append(x);
                return res.toString();
            case popn:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("02");
                res.append(x);
                return res.toString();
            case loca:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("0a");
                res.append(x);
                return res.toString();
            case arga:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("0b");
                res.append(x);
                return res.toString();
            case globa:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("0c");
                res.append(x);
                return res.toString();
            case stackalloc:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("1a");
                res.append(x);
                return res.toString();
            case br:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("41");
                res.append(x);
                return res.toString();
            case brfalse:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("42");
                res.append(x);
                return res.toString();
            case brtrue:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("43");
                res.append(x);
                return res.toString();
            case call:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("48");
                res.append(x);
                return res.toString();
            case callname:
                x = Integer.toHexString(ins.getX());
                x = hexaZeroFill(x, 8);
                res.append("4a");
                res.append(x);
                return res.toString();
            default:
                return null;
        }
    }
}

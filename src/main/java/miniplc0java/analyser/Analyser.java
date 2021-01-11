package miniplc0java.analyser;

import miniplc0java.error.AnalyzeError;
import miniplc0java.error.CompileError;
import miniplc0java.error.ErrorCode;
import miniplc0java.error.ExpectedTokenError;
import miniplc0java.error.TokenizeError;
import miniplc0java.instruction.Instruction;
import miniplc0java.instruction.Operation;
import miniplc0java.tokenizer.Token;
import miniplc0java.tokenizer.TokenType;
import miniplc0java.tokenizer.Tokenizer;
import miniplc0java.util.Pos;
import org.checkerframework.checker.units.qual.A;

import java.sql.SQLOutput;
import java.util.*;

public final class Analyser {

    Tokenizer tokenizer;
    ArrayList<Instruction> instructions;

    /** 当前偷看的 token */
    Token peekedToken = null;

    /** 符号表 */
    Stack<SymbolEntry> symbolTable = new Stack<>();

    /** 下一个变量的栈偏移 */
    int nextOffset = 0;

    /** 全局变量数 */
    int globCount = 0;

    /** 函数个数 */
    int funcCount = 0;

    /** 标准库函数个数 */
    int stdCount = 0;

    /** 局部变量数 */
    int localCount = 0;

    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
        this.instructions = new ArrayList<>();
    }

    public List<Object> analyse() throws CompileError {
        analyseProgram();
        List<Object> commands = generateCode();
        return commands;
    }

    private List<Object> generateCode() {
        List<Object> commands = new ArrayList<>();

        commands.add(globCount);
        // 全局变量数组
        for (SymbolEntry se:
                symbolTable) {
            if (se.isGlobal()) {
                commands.add(se.isConstant ? 1 : 0);
                commands.add(se.getBitLength());
                if (se.isFunction() || se.getType() == "string")
                    commands.add(se.getName());
                else
                    commands.add(0);
            }
        }

        // 全局变量表与函数表 分隔符
        commands.add("#");

        commands.add(funcCount + 1);
        // 函数数组
        for (SymbolEntry se:
                symbolTable) {
            if (se.isFunction() && !isStd(se.getName())) {
                commands.add(se.getGlobalIndex());
                commands.add(se.getType() == "void" ? 0 : 1);
                commands.add(se.getParamSlots());
                commands.add(se.getLocSlots());
                commands.add(se.getBodyCount());
                for (Instruction cm:
                        instructions) {
                    if (cm.getFuncIndex() == se.getFunctionIndex())
                        commands.add(cm);
                }
            }
        }

        for(Object o:
            commands) {
            System.out.println(o.toString());
        }

        return commands;
    }

    /** 测试使用 */
    public ArrayList<Instruction> getInstructions() {
        return instructions;
    }

    /**
     * 查看下一个 Token
     * 
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token
     * 
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            var token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     * 
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        var token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     * 
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     * 
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 获取下一个变量的栈偏移
     * 
     * @return
     */
    private int getNextVariableOffset() {
        return this.nextOffset++;
    }

    /**
     * 按名称搜索符号表
     *
     * @param name          名字
     */
    private SymbolEntry getSymbolEntryByName(String name) {
        for (SymbolEntry SE:
             this.symbolTable) {
            if (SE.getName().equals(name))
                return SE;
        }
        return null;
    }

    /**
     * 按名称搜索符号表中的全局变量
     *
     * @param name 名称
     */
    private SymbolEntry getGlobalSymbolEntry(String name) {
        for (SymbolEntry se:
                this.symbolTable) {
            // 由于表示函数参数时，借用 GlobalIndex 属性
            // 因此在查找全局变量时，应排除 LocalIndex 不为 -1 的符号
            if (se.getName().equals(name) && se.isGlobal() && !se.isLocal())
                return se;
        }
        return null;
    }

    /**
     * 按名称搜索符号表中的局部变量
     *
     * @param name 名称
     */
    private SymbolEntry getLocalSymbolEntry(String name) {
        for (SymbolEntry se:
                this.symbolTable) {
            if (se.getName().equals(name) && !se.isGlobal() && se.isLocal())
                return se;
        }
        return null;
    }

    /**
     * 按名称搜索符号表中的函数定义
     *
     * @param name 名称
     */
    private SymbolEntry getFuncSymbolEntry(String name) {
        for (SymbolEntry se:
                this.symbolTable) {
            if (se.getName().equals(name) && se.isFunction())
                return se;
        }
        return null;
    }

    /**
     * 按名称搜索符号表中的静态变量
     *
     * @param name 名称
     */
    private SymbolEntry getConstSymbolEntry(String name) {
        for (SymbolEntry se:
                this.symbolTable) {
            if (se.getName().equals(name) && se.isConstant())
                return se;
        }
        return null;
    }

    /**
     * 添加一个符号
     * 
     * @param name          名字
     * @param isInitialized 是否已赋值
     * @param isConstant    是否是常量
     * @param curPos        当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private void addSymbol(String name, String type, Integer localIndex, Integer GlobalIndex,
                           Integer FunctionIndex, boolean isFuncIdent, boolean isInitialized,
                           boolean isConstant, Pos curPos) throws AnalyzeError {
        if (getSymbolEntryByName(name) != null) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        } else {
            this.symbolTable.push(new SymbolEntry(name, type, localIndex, GlobalIndex,
                    FunctionIndex, isFuncIdent, isConstant, isInitialized, getNextVariableOffset()));
        }
    }

    /**
     * 函数返回后，删除符号表中的局部变量
     *
     */
    private void deleteSymbolByRet() {
        for (int i = 0; i < symbolTable.size(); i++) {
            var se = symbolTable.get(i);
            if (se.getFunctionIndex() == funcCount && !se.isFunction() && !se.isGlobal()){
                if (symbolTable.peek().equals(se))
                    symbolTable.pop();
                else
                    symbolTable.remove(se);
            }
        }
    }

    /**
     * 设置符号为已赋值
     * 
     * @param name   符号名称
     * @param curPos 当前位置（报错用）
     * @throws AnalyzeError 如果未定义则抛异常
     */
    private void initializeSymbol(String name, Pos curPos) throws AnalyzeError {
        var entry = getSymbolEntryByName(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            entry.setInitialized(true);
        }
    }

    /**
     * 获取变量在栈上的偏移
     * 
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 栈偏移
     * @throws AnalyzeError
     */
    private int getOffset(String name, Pos curPos) throws AnalyzeError {
        var entry = getSymbolEntryByName(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.getStackOffset();
        }
    }

    /**
     * 获取变量的类型
     *
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 栈偏移
     * @throws AnalyzeError
     */
    private String getSymbolType(String name, Pos curPos) throws AnalyzeError {
        var entry = getSymbolEntryByName(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.getType();
        }
    }

    /**
     * 获取变量是否是常量
     * 
     * @param name   符号名
     * @param curPos 当前位置（报错用）
     * @return 是否为常量
     * @throws AnalyzeError
     */
    private boolean isConstant(String name, Pos curPos) throws AnalyzeError {
        var entry = getSymbolEntryByName(name);
        if (entry == null) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        } else {
            return entry.isConstant();
        }
    }

    /**
     * 主程序入口
     *
     * @throws CompileError
     */
    private void analyseProgram() throws CompileError {
        // program -> decl_stmt* function*
        var peekedToken = peek();

        // 分析全局变量
        while (peekedToken.getTokenType() == TokenType.CONST_KW ||
                peekedToken.getTokenType() == TokenType.LET_KW) {
            analyseDeclareStatement(true);
            peekedToken = peek();
        }

        var expectedFuncPos = peekedToken.getStartPos();

        // 符号表中插入入口函数
        addSymbol("_start", "void", -1, globCount, 0,
                true, true, true, expectedFuncPos);

        // 分析函数定义
        while (peekedToken.getTokenType() == TokenType.FN_KW) {
            analyseFunction();
            peekedToken = peek();
        }

        // 判断有无main函数
        var mainfunc = getFuncSymbolEntry("main");
        if (mainfunc == null)
            throw new AnalyzeError(ErrorCode.NoEntry, expectedFuncPos);
        else {
            instructions.add(new Instruction(Operation.stackalloc, getStackAlloc(mainfunc.getType()), 0));
            instructions.add(new Instruction(Operation.call, mainfunc.getFunctionIndex(), 0));
            var popNum = popN(0);
            if (popNum > 0) {
                instructions.add(new Instruction(Operation.popn, popNum, 0));
            }
            var bodyCount = 0;
            for (Instruction ins:
                 instructions) {
                if (ins.getFuncIndex() == 0)
                    bodyCount++;
            }
            getFuncSymbolEntry("_start").setBodyCount(bodyCount);
        }

        expect(TokenType.EOF);
    }

    private String analyseType() throws CompileError {
        var nameToken = expect(TokenType.IDENT);

        switch ((String) nameToken.getValue()) {
            case "int":
                return "int";
            case "void":
                return "void";
            case "double":
                return "double";
            default:
                throw new AnalyzeError(ErrorCode.InvalidIdentifier, nameToken.getStartPos());
        }
    }

    private Boolean analyseStatement(Boolean funcNeedRet) throws CompileError {
        var peekedToken = peek();

        if (peekedToken.getTokenType() == TokenType.LET_KW ||
                peekedToken.getTokenType() == TokenType.CONST_KW) {
            analyseDeclareStatement(false);
        }
        else if (peekedToken.getTokenType() == TokenType.IF_KW) {
            analyseIfStatement();
        }
        else if (peekedToken.getTokenType() == TokenType.WHILE_KW) {
            analyseWhileStatement();
        }
        else if (peekedToken.getTokenType() == TokenType.RETURN_KW) {
            analyseReturnStatement(funcNeedRet);
            return true;
        }
        else if (peekedToken.getTokenType() == TokenType.L_BRACE) {
            analyseBlockStatement(false);
        }
        else if (peekedToken.getTokenType() == TokenType.SEMICOLON){
            expect(TokenType.SEMICOLON);
        }
        else {
            analyseExpressionStatement();
        }

        return false;
    }

    private void analyseExpressionStatement() throws CompileError {
        analyseOPG();

        expect(TokenType.SEMICOLON);
    }

    private void analyseDeclareStatement(Boolean isGlobal) throws CompileError {
        if(check(TokenType.LET_KW))
            analyseLetDeclareStatement(isGlobal);
        else if (check(TokenType.CONST_KW))
            analyseConstDeclareStatement(isGlobal);
        else
            throw new AnalyzeError(ErrorCode.InvalidIdentifier, peek().getStartPos());
    }

    private void analyseLetDeclareStatement(Boolean isGlobal) throws CompileError {
        expect(TokenType.LET_KW);

        var nameToken = expect(TokenType.IDENT);

        expect(TokenType.COLON);

        var type = analyseType();

        boolean initialized = check(TokenType.ASSIGN);

        String name = (String) nameToken.getValue();
        if (isGlobal) {
            globCount++;
            addSymbol(name, type, -1, globCount, funcCount,
                    false, initialized, false, nameToken.getStartPos());
            if (initialized)
                instructions.add(new Instruction(Operation.globa,
                        getGlobalSymbolEntry(name).getGlobalIndex(), funcCount));
        }
        else {
            addSymbol(name, type, localCount, -1, funcCount,
                    false, initialized, false, nameToken.getStartPos());
            if (initialized)
                instructions.add(new Instruction(Operation.loca, getLocalSymbolEntry(name).getLocalIndex(), funcCount));
            localCount++;
        }

        if (initialized) {
            expect(TokenType.ASSIGN);
            analyseOPG();
            instructions.add(new Instruction(Operation.store64, funcCount));
        }

        expect(TokenType.SEMICOLON);
    }

    private void analyseConstDeclareStatement(Boolean isGlobal) throws CompileError {
        expect(TokenType.CONST_KW);

        var nameToken = expect(TokenType.IDENT);

        expect(TokenType.COLON);

        var type = analyseType();

        expect(TokenType.ASSIGN);

        String name = (String) nameToken.getValue();
        if (isGlobal) {
            globCount++;
            addSymbol(name, type, -1, globCount, funcCount,
                    false, true, true, nameToken.getStartPos());
            instructions.add(new Instruction(Operation.globa, getGlobalSymbolEntry(name).getGlobalIndex(), funcCount));
        }
        else {
            addSymbol(name, type, localCount, -1, funcCount,
                    false,true, true, nameToken.getStartPos());
            instructions.add(new Instruction(Operation.loca, localCount, funcCount));
            localCount++;
        }

        analyseOPG();

        instructions.add(new Instruction(Operation.store64, funcCount));

        expect(TokenType.SEMICOLON);
    }

    private void analyseIfStatement() throws CompileError {
        expect(TokenType.IF_KW);

        analyseOPG();

        var begin = instructions.size();

        instructions.add(new Instruction(Operation.br, 0, funcCount));

        analyseBlockStatement(false);

        var end = instructions.size();
        var ifLength = end - begin;

        // 回填
        for (int i = end - 1; i >= 0; i--) {
            var cm = instructions.get(i);
            if (cm.getOpt().toString() == "br") {
                cm.setX(ifLength);
                break;
            }
        }

        instructions.add(new Instruction(Operation.br, 0, funcCount));

        if (nextIf(TokenType.ELSE_KW) != null) {
            if (check(TokenType.L_BRACE))
                analyseBlockStatement(false);
            else if(check(TokenType.IF_KW))
                analyseIfStatement();
        }
    }

    private void analyseWhileStatement() throws CompileError {
        expect(TokenType.WHILE_KW);

        instructions.add(new Instruction(Operation.br, 0, funcCount));

        var begin = instructions.size();

        analyseOPG();

        var afterJudge = instructions.size();

        instructions.add(new Instruction(Operation.br, 0, funcCount));

        analyseBlockStatement(false);

        var end = instructions.size();
        var loopLength = end - afterJudge;
        var backforward = begin - end - 1;

        // 回填
        for (int i = end - 1; i >= 0; i--) {
            var cm = instructions.get(i);
            if (cm.getOpt().toString() == "br") {
                cm.setX(loopLength);
                break;
            }
        }

        instructions.add((new Instruction(Operation.br, backforward, funcCount)));
    }

    private void analyseReturnStatement(Boolean funcNeedRet) throws CompileError {
        expect(TokenType.RETURN_KW);

        if (funcNeedRet)
            instructions.add(new Instruction(Operation.arga, 0, funcCount));

        if (nextIf(TokenType.SEMICOLON) == null) {
            analyseOPG();
            expect(TokenType.SEMICOLON);
            instructions.add(new Instruction(Operation.store64, funcCount));
        }

        instructions.add(new Instruction(Operation.ret, funcCount));
    }

    private Boolean analyseBlockStatement(Boolean funcNeedRet) throws CompileError {
        expect(TokenType.L_BRACE);

        // 确认类型函数最后一个语句为返回语句
        var isRet = false;

        while (nextIf(TokenType.R_BRACE) == null) {
            isRet = false;
            if (analyseStatement(funcNeedRet))
                isRet = true;
        }

        return isRet;
    }

    private void analyseFunction() throws CompileError {
        // 识别到函数， funcCount + 1
        globCount ++;
        funcCount ++;

        expect(TokenType.FN_KW);

        var nameToken = expect(TokenType.IDENT);

        expect(TokenType.L_PAREN);

        if (nextIf(TokenType.R_PAREN) == null) {
            analyseFunctionParamList();
            expect(TokenType.R_PAREN);
        }

        expect(TokenType.ARROW);

        var type = analyseType();
        Boolean needRet = type == "void"? false: true;

        String name = (String) nameToken.getValue();
        addSymbol(name, type, -1, globCount, funcCount,
                true, true, true, nameToken.getStartPos());

        // localCount 仅用于辅助判断是否为参数，分析完毕后归零
        var _this = getFuncSymbolEntry((String) nameToken.getValue());
        _this.setParamSlots(localCount);
        localCount = 0;

        // 记录函数体指令长度
        var begin = instructions.size();

        var isRet = analyseBlockStatement(needRet);

        var end = instructions.size();

        _this.setLocSlots(localCount);


        // 返回前检查是否需要 popN
        var popNum = popN(funcCount);
        if (popNum > 0)
            instructions.add(new Instruction(Operation.popn, popNum, funcCount));

        // 函数返回，localCount 归零
        if (!isRet) {
            // void函数，添加ret指令
            if (!needRet) {
                instructions.add(new Instruction(Operation.ret, funcCount));
                _this.setBodyCount(end - begin + 1);
            }
            else throw new AnalyzeError(ErrorCode.FunctionNeedReturn, peek().getStartPos());
        }
        else _this.setBodyCount(end - begin);
        deleteSymbolByRet();
        localCount = 0;
    }

    private void analyseFunctionParamList() throws CompileError {
        // 参数序号从 1 开始， 0 为返回值
        var argNum = 1;

        analyseFunctionParam(argNum++);

        while (check(TokenType.COMMA)) {
            expect(TokenType.COMMA);

            // 参数序号 + 1
            analyseFunctionParam(argNum++);
        }
    }

    private void analyseFunctionParam(Integer argNum) throws CompileError {
        boolean isConst = check(TokenType.CONST_KW);

        if (isConst)
            expect(TokenType.CONST_KW);

        var nameToken = expect(TokenType.IDENT);

        expect(TokenType.COLON);

        var type = analyseType();

        // 此处的 GlobalIndex = argNum 代表 函数的第几个参数
        String name = (String) nameToken.getValue();
        addSymbol(name, type, localCount++, argNum, funcCount,
                false, false, isConst, nameToken.getStartPos());
    }

    private void analyseAssignExpr(Token l_expr) throws CompileError {
        // 变量名
        var nameToken = l_expr;

        expect(TokenType.ASSIGN);

        String name = (String) nameToken.getValue();
        var symbol = getSymbolEntryByName(name);
        if (symbol == null) {
            // 没有这个标识符
            throw new AnalyzeError(ErrorCode.NotDeclared, nameToken.getStartPos());
        } else if (symbol.isConstant) {
            // 标识符是常量
            throw new AnalyzeError(ErrorCode.AssignToConstant, nameToken.getStartPos());
        }

        // 设置符号已初始化
        if (symbol.isInitialized == false)
            initializeSymbol(name, nameToken.getStartPos());

        if (symbol.isGlobal())
            instructions.add(new Instruction(Operation.globa, symbol.getGlobalIndex(), funcCount));
        else
            instructions.add(new Instruction(Operation.loca, symbol.getLocalIndex(), funcCount));

        analyseOPG();

        instructions.add(new Instruction(Operation.store64, funcCount));
    }

    private void analyseCallExpr(Token fn_nameToken) throws CompileError {
        // call_expr -> IDENT '(' call_param_list? ')'
        String fn_name = (String) fn_nameToken.getValue();

        expect(TokenType.L_PAREN);

        // 如果是标准库函数，直接生成对应指令
        var isStdFunc= stdFunc(fn_nameToken, true);

        // 如果不是标准库函数
        if (isStdFunc == null) {
            // 分配返回地址空间
            var type= getFuncSymbolEntry(fn_name).getType();
            instructions.add(new Instruction(Operation.stackalloc, getStackAlloc(type), funcCount));

            analyseCallParamList();

            // 调用
            var funcIndex = getFuncSymbolEntry(fn_name).getFunctionIndex();
            instructions.add(new Instruction(Operation.call, funcIndex, funcCount));
        }

        expect(TokenType.R_PAREN);
    }

    private void analyseCallParamList() throws CompileError {
        analyseOPG();

        while (check(TokenType.COMMA)) {
            expect(TokenType.COMMA);

            analyseOPG();
        }
    }

    public void analyseOPG() throws CompileError {
        // 符号栈
        Stack<Token> symbolStack = new Stack<>();

        symbolStack.push(new Token(TokenType.END, '#', new Pos(0,0), new Pos(0,0)));

        Token curOut = null;
        // 如果是',' or ';' or '{' 停止读取
        if (check(TokenType.COMMA) || check(TokenType.SEMICOLON)
                || check(TokenType.L_BRACE)) {
            curOut = new Token(TokenType.END, '#', new Pos(0,0), new Pos(0,0));
        }
        else curOut = next();
        var curIn = symbolStack.peek();
        var prevOut = new Token(TokenType.END, '#', new Pos(0,0), new Pos(0,0));

        // 循环指示器
        boolean QF = false; // quit flag 退出指示器，表示退出冲动
        boolean PF = true; // prune flag 规约指示器，表示是否可以继续规约

        if (curOut.getTokenType() == TokenType.END) {
            QF = true;
            PF = false;
        }

        while(!QF || PF) {
            // 判断是否为取负符号
            if (curOut.getTokenType() == TokenType.MINUS) {
                if (prevOut.getTokenType() == TokenType.END)
                    curOut.setTokenType(TokenType.NEGATE);
                else if (prevOut.getTokenType() != TokenType.L_PAREN && prevOut.getTokenType() != TokenType.UINT &&
                        prevOut.getTokenType() != TokenType.STRING && prevOut.getTokenType() != TokenType.IDENT ){
                    curOut.setTokenType(TokenType.NEGATE);
                }
            }
            // in<out || in=out 移进
            if (OPG.compare(curIn, curOut) == 2 ||
                    OPG.compare(curIn, curOut) == 1){
                if (curOut.getTokenType() == TokenType.IDENT) {
                    // 判断是否占用了标准库函数
                    if (stdFunc(curOut, false) != null) {
                        if (peek().getTokenType() != TokenType.L_PAREN)
                            throw new AnalyzeError(ErrorCode.ExpectedToken, curOut.getStartPos());
                        else analyseCallExpr(curOut);
                    }
                    // 判断是否为函数调用
                    if (peek().getTokenType() == TokenType.L_PAREN) {
                        if (getFuncSymbolEntry((String) curOut.getValue()) != null)
                            analyseCallExpr(curOut);
                        else throw new AnalyzeError(ErrorCode.ExpectedToken, curOut.getStartPos());
                    }
                    // 判断是否为赋值表达式
                    else if (peekedToken.getTokenType() == TokenType.ASSIGN) {
                        analyseAssignExpr(curOut);
                        symbolStack.push(new Token(TokenType.NONE, null, new Pos(0,0), new Pos(0,0)));
                        QF = true;
                        PF = false;
                        continue;
                    }
                }
                symbolStack.push(curOut);
                curIn = symbolStack.peek();
                prevOut = curOut;
                // 下一个如果是')'，判断是否继续读取
                if (check(TokenType.R_PAREN) && !QF) {
                    // 假设不读取
                    QF = true;
                    // 查看栈内括号表达式是否已规约
                    for (Token t:
                            symbolStack) {
                        // 还有未规约的'('，继续读取
                        if (t.getTokenType() == TokenType.L_PAREN)
                            QF = false;
                    }
                }
                // 如果是',' or ';' or '{' 停止读取
                if (check(TokenType.COMMA) || check(TokenType.SEMICOLON)
                        || check(TokenType.L_BRACE) || QF) {
                    curOut = new Token(TokenType.END, '#', new Pos(0,0), new Pos(0,0));
                }
                else curOut = next();
            }
            // in>out 规约
            else if (OPG.compare(curIn, curOut) == 3){
                switch (OPG.getSymbol(curIn.getTokenType())) {
                    case "+":
                    case "-":
                    case "*":
                    case "/":
                    case ">":
                    case "<":
                    case ">=":
                    case "<=":
                    case "==":
                    case "!=":
                        // N + N 弹出3个符号，推入1个N
                    case "(":
                    case ")":
                        // (N) 弹出3个符号，推入一个N
                    case "as":
                        // N as N 弹出3个符号，推入一个N
                        symbolStack.pop();
                        var midOp = symbolStack.pop();
                        symbolStack.pop();

                        // 生成指令
                        switch (midOp.getTokenType()) {
                            case PLUS:
                                instructions.add(new Instruction(Operation.addi, funcCount));
                                break;
                            case MINUS:
                                instructions.add(new Instruction(Operation.subi, funcCount));
                                break;
                            case MUL:
                                instructions.add(new Instruction(Operation.muli, funcCount));
                                break;
                            case DIV:
                                instructions.add(new Instruction(Operation.divi, funcCount));
                                break;
                            case GT:
                                instructions.add(new Instruction(Operation.cmpi, funcCount));
                                instructions.add(new Instruction(Operation.setgt, funcCount));
                                instructions.add(new Instruction(Operation.brtrue, 1, funcCount));
                                break;
                            case LT:
                                instructions.add(new Instruction(Operation.cmpi, funcCount));
                                instructions.add(new Instruction(Operation.setlt, funcCount));
                                instructions.add(new Instruction(Operation.brtrue, 1, funcCount));
                                break;
                            case GE:
                                instructions.add(new Instruction(Operation.cmpi, funcCount));
                                instructions.add(new Instruction(Operation.setlt, funcCount));
                                instructions.add(new Instruction(Operation.brfalse, 1, funcCount));
                                break;
                            case LE:
                                instructions.add(new Instruction(Operation.cmpi, funcCount));
                                instructions.add(new Instruction(Operation.setgt, funcCount));
                                instructions.add(new Instruction(Operation.brfalse, 1, funcCount));
                                break;
                            case EQ:
                                instructions.add(new Instruction(Operation.cmpi, funcCount));
                                instructions.add(new Instruction(Operation.brfalse, 1, funcCount));
                                break;
                            case NEQ:
                                instructions.add(new Instruction(Operation.cmpi, funcCount));
                                instructions.add(new Instruction(Operation.brtrue, 1, funcCount));
                            default:
                                break;
                        }

                        symbolStack.push(new Token(TokenType.NONE, null,
                                new Pos(0,0), new Pos(0,0)));
                        break;
                    case "i":
                        // i -> N, 弹出i，推入n
                        var nameToken = symbolStack.pop();

                        // 生成指令
                        switch (nameToken.getTokenType()) {
                            case UINT:
                                instructions.add(new Instruction(Operation.push,
                                        (Integer) nameToken.getValue(), funcCount));
                                break;
                            case STRING:
                                var StringVar = getGlobalSymbolEntry((String) nameToken.getValue());
                                if (StringVar == null) {
                                    globCount++;
                                    addSymbol((String) nameToken.getValue(), "string",
                                            -1, globCount, funcCount, false,
                                            true, true, nameToken.getStartPos());
                                    StringVar = getGlobalSymbolEntry((String) nameToken.getValue());
                                }
                                instructions.add(new Instruction(Operation.push, StringVar.getGlobalIndex()));
                                break;
                            case IDENT:
                                var SE = getSymbolEntryByName((String) nameToken.getValue());
                                if (SE != null && !SE.isFunction()) {
                                    if (SE.isGlobal()) {
                                        // 函数参数
                                        if (SE.isLocal()) {
                                            instructions.add(new Instruction(Operation.arga,
                                                    SE.getGlobalIndex(), funcCount));
                                        }
                                        // 全局变量
                                        else {
                                            instructions.add(new Instruction(Operation.globa,
                                                    SE.getGlobalIndex(), funcCount));
                                        }
                                    }
                                    else
                                        instructions.add(new Instruction(Operation.loca,
                                                SE.getLocalIndex(), funcCount));

                                    instructions.add(new Instruction(Operation.load64, funcCount));
                                }
                            default:
                                break;
                        }

                        symbolStack.push(new Token(TokenType.NONE, null,
                                new Pos(0,0), new Pos(0,0)));
                        break;
                    case "n":
                        // -N 弹出2个符号，推入一个N
                        symbolStack.pop();
                        symbolStack.pop();

                        instructions.add(new Instruction(Operation.negi, funcCount));

                        symbolStack.push(new Token(TokenType.NONE, null,
                                new Pos(0,0), new Pos(0,0)));
                        break;
                    case "#":
                        // 符号栈中终结符只剩下#，不能规约，退出循环
                        PF = false;
                        QF = true;
                        break;
                    default:
                        // 不认识，摸了
                        throw new AnalyzeError(ErrorCode.ExpectedToken, curOut.getStartPos());
                }

                curIn = OPG.getOpIn(symbolStack);
            }
            // 表项不存在,报错
            else
                throw new AnalyzeError(ErrorCode.ExpectedToken, curOut.getStartPos());
        }

        if (symbolStack.peek().getTokenType() != TokenType.NONE || symbolStack.size() != 2)
            throw new AnalyzeError(ErrorCode.ExpectedToken, curOut.getStartPos());
    }

    private SymbolEntry stdFunc(Token fn_nameToken, Boolean genInstructions) throws CompileError {
        SymbolEntry isDeclared = null;

        switch ((String) fn_nameToken.getValue()) {
            case "getint":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("getint");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("getint", "int", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("getint");
                }
                instructions.add(new Instruction(Operation.stackalloc, 1, funcCount));
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("getint", "int", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("getint", fn_nameToken.getStartPos()));
            case "getdouble":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("getdouble");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("getdouble", "double", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("getdouble");
                }
                instructions.add(new Instruction(Operation.stackalloc, 1, funcCount));
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("getdouble", "double", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("getdouble", fn_nameToken.getStartPos()));
            case "getchar":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("getchar");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("getchar", "int", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("getchar");
                }
                instructions.add(new Instruction(Operation.stackalloc, 1, funcCount));
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("getchar", "int", -1, -1,
                        funcCount+stdCount, true, true, true,
                        getOffset("getchar", fn_nameToken.getStartPos()));
            case "putint":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", globCount,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("putint");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("putint", "void", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("putint");
                }
                instructions.add(new Instruction(Operation.stackalloc, 0, funcCount));
                analyseCallParamList();
                instructions.add(new Instruction(Operation.callname, isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("putint", "void", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("putint", fn_nameToken.getStartPos()));
            case "putdouble":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("putdouble");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("putdouble", "void", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("putdouble");
                }
                instructions.add(new Instruction(Operation.stackalloc, 0, funcCount));
                analyseCallParamList();
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("putdouble", "void", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("putdouble", fn_nameToken.getStartPos()));
            case "putchar":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("putchar");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("putchar", "int", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("putchar");
                }
                instructions.add(new Instruction(Operation.stackalloc, 0, funcCount));
                analyseCallParamList();
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("putchar", "void", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("putchar", fn_nameToken.getStartPos()));
            case "putstr":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("putstr");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("putstr", "void", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("putstr");
                }
                instructions.add(new Instruction(Operation.stackalloc, 0, funcCount));
                analyseCallParamList();
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("putstr", "void", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("putstr", fn_nameToken.getStartPos()));
            case "putln":
                if (!genInstructions) return new SymbolEntry("true", "Boolean", -1,
                        -1, -1, false, false, false, 0);

                isDeclared =  getFuncSymbolEntry("putln");
                if (isDeclared == null) {
                    stdCount++;
                    globCount++;
                    addSymbol("putln", "void", -1, globCount, funcCount+stdCount,
                            true, true, true, fn_nameToken.getStartPos());
                    isDeclared =  getFuncSymbolEntry("putln");
                }
                instructions.add(new Instruction(Operation.stackalloc, 0, funcCount));
                instructions.add(new Instruction(Operation.callname,  isDeclared.getGlobalIndex(), funcCount));
                return new SymbolEntry("putln", "void", -1, globCount,
                        funcCount+stdCount, true, true, true,
                        getOffset("putln", fn_nameToken.getStartPos()));
            default:
                return null;
        }
    }

    private Boolean isStd(String name) {
        switch (name) {
            case "getint":
            case "getdouble":
            case "getchar":
            case "putint":
            case "putdouble":
            case "putchar":
            case "putstr":
            case "putln":
                return true;
            default:
                return false;
        }
    }

    private int getStackAlloc(String type) {
        if (!"void".equals(type))
            return 1;
        else
            return 0;
    }

    private Integer popN(Integer funcCount) {
        var popNum = 0;
        var prevNum = -1;

        for (Instruction cm:
             instructions) {
            if (cm.getFuncIndex() == funcCount) {
                switch (cm.getOpt()) {
                    case pop:
                    case free:
                    case addi:
                    case subi:
                    case muli:
                    case divi:
                    case addf:
                    case subf:
                    case mulf:
                    case divf:
                    case divu:
                    case shl:
                    case shr:
                    case and:
                    case or:
                    case xor:
                    case cmpi:
                    case cmpu:
                    case cmpf:
                    case shrl:
                    case printi:
                    case printc:
                    case printf:
                    case prints:
                    case brfalse:
                    case brtrue:
                        popNum--;break;
                    case store8:
                    case store16:
                    case store32:
                    case store64:
                        popNum--;popNum--;break;
                    case dup:
                    case scani:
                    case scanc:
                    case scanf:
                    case push:
                    case loca:
                    case arga:
                    case globa:
                        popNum++;break;
                    case popn:
                        popNum -= cm.getX();
                        break;
                    case stackalloc:
                        // 分配返回地址时，记录栈内空间的大小
                        if (prevNum > popNum + cm.getX() || prevNum == -1)
                            prevNum = popNum + cm.getX();
                        popNum += cm.getX();
                        break;
                    case call:
                    case callname:
                        // 当发生函数调用并成功返回后，栈内空间大小恢复到原大小
                        popNum = prevNum;
                    default:
                        break;
                }
            }
        }

        return popNum;
    }
}

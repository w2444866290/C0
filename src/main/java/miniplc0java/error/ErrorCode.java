package miniplc0java.error;

public enum ErrorCode {
    NoError, // Should be only used internally.
    StreamError, EOF, InvalidInput, InvalidIdentifier, InvalidStringValue, IntegerOverflow, // int32_t overflow.
    NoEntry, FunctionNeedReturn,  NeedIdentifier, ConstantNeedValue, NoSemicolon, InvalidVariableDeclaration,
    IncompleteExpression, NotDeclared, AssignToConstant, DuplicateDeclaration, NotInitialized, InvalidAssignment,
    InvalidPrint, ExpectedToken
}

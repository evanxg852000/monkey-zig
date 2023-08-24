const std = @import("std");

const token = @import("./token.zig");
const lexer = @import("./lexer.zig");
const ast = @import("./ast.zig");
const utils = @import("./utils.zig");

const Allocator = std.mem.Allocator;
const fmt = std.fmt;
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;
const testing = std.testing;

const ParserError = error{
    UnknownConstruct,
    UnexpectedToken,
    ConversionFailed,
};

const OperatorPrecedence = enum(u16) {
    lowest = 0,
    equals, // ==
    lessGreater, // > or <
    sum, // +
    product, // *
    prefix, // -x or !x
    call, // foo(x)
};

const prefixParseFn = *const fn (*Parser) anyerror!ast.Expression;
const infixParseFn = *const fn (*Parser, *ast.Expression) anyerror!ast.Expression;

pub const Parser = struct {
    const Self = @This();

    allocator: Allocator,
    lexer: *lexer.Lexer,
    currentToken: token.Token,
    peekToken: token.Token,
    program: ast.Program,
    errors: ArrayList([]const u8),
    prefixParseFns: AutoHashMap(token.TokenType, prefixParseFn),
    infixParseFns: AutoHashMap(token.TokenType, infixParseFn),

    pub fn new(allocator: Allocator, lxr: *lexer.Lexer) Self {
        var parser = Self{
            .allocator = allocator,
            .lexer = lxr,
            .currentToken = token.Token.illegal(),
            .peekToken = token.Token.illegal(),
            .program = ast.Program.init(allocator),
            .errors = ArrayList([]const u8).init(allocator),
            .prefixParseFns = AutoHashMap(token.TokenType, prefixParseFn).init(allocator),
            .infixParseFns = AutoHashMap(token.TokenType, infixParseFn).init(allocator),
        };

        // register parserFns
        // TODO: fix sloppy error handling (for now let's just keep moving)
        parser.prefixParseFns.put(token.TokenType.ident, Self.parseIdentifier) catch {};
        parser.prefixParseFns.put(token.TokenType.int, Self.parseIntegerLiteral) catch {};
        parser.prefixParseFns.put(token.TokenType.bang, Self.parsePrefixExpression) catch {};
        parser.prefixParseFns.put(token.TokenType.minus, Self.parsePrefixExpression) catch {};
        parser.prefixParseFns.put(token.TokenType.true_, Self.parseBooleanExpression) catch {};
        parser.prefixParseFns.put(token.TokenType.false_, Self.parseBooleanExpression) catch {};
        parser.prefixParseFns.put(token.TokenType.lparen, Self.parseGroupedExpression) catch {};
        parser.prefixParseFns.put(token.TokenType.if_, Self.parseIfExpression) catch {};
        parser.prefixParseFns.put(token.TokenType.function, Self.parseFuntionLiteral) catch {};

        parser.infixParseFns.put(token.TokenType.plus, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.minus, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.asterisk, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.slash, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.eq, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.noteq, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.lt, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.gt, Self.parseInfixExpression) catch {};
        parser.infixParseFns.put(token.TokenType.lparen, Self.parseCallExpression) catch {};

        // Read two tokens to initialize current and peek tokens.
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Self) !*ast.Program {
        while (!self.currentTokenIs(token.TokenType.eof)) {
            const statement = try self.parseStatement();
            try self.program.statements.append(statement);
            self.nextToken();
        }
        return &self.program;
    }

    pub fn deinit(self: *Self) void {
        for (self.errors.items) |item| {
            self.allocator.free(item);
        }
        self.errors.clearAndFree();
        self.prefixParseFns.clearAndFree();
        self.infixParseFns.clearAndFree();

        for (self.program.statements.items) |stmt| {
            switch (stmt) {
                .exprStmt => |exprStmt| {
                    freeExprPointer(self.allocator, exprStmt.expr);
                },
                else => {},
            }
        }
        self.program.statements.clearAndFree();
    }

    pub fn getErrors(self: *Self) [][]const u8 {
        return self.errors.items;
    }

    fn parseStatement(self: *Self) !ast.Statement {
        return switch (self.currentToken.tokenType) {
            .let => ast.Statement{
                .letStmt = try self.parseLetStatement(),
            },
            .return_ => ast.Statement{
                .returnStmt = try self.parserReturnStatement(),
            },
            else => ast.Statement{
                .exprStmt = try self.parseExpressionStatement(),
            },
        };
    }

    fn parseLetStatement(self: *Self) !ast.LetStatement {
        var stmt = ast.LetStatement{
            .token = self.currentToken,
            .name = undefined,
            .value = undefined,
        };

        try self.expectPeek(token.TokenType.ident);

        stmt.name = ast.Identifier{
            .token = self.currentToken,
            .value = self.currentToken.literal,
        };

        try self.expectPeek(token.TokenType.assign);
        self.nextToken();

        stmt.value = try self.parseExpression(@intFromEnum(OperatorPrecedence.lowest));

        if (self.peekTokenIs(token.TokenType.semicolon)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parserReturnStatement(self: *Self) !ast.ReturnStatement {
        var stmt = ast.ReturnStatement{
            .token = self.currentToken,
            .value = undefined,
        };

        self.nextToken();
        stmt.value = try self.parseExpression(@intFromEnum(OperatorPrecedence.lowest));

        if (self.peekTokenIs(token.TokenType.semicolon)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseExpressionStatement(self: *Self) !ast.ExpressionStatement {
        var stmt = ast.ExpressionStatement{
            .token = self.currentToken,
            .expr = try self.parseExpression(@intFromEnum(OperatorPrecedence.lowest)),
        };

        if (self.peekTokenIs(token.TokenType.semicolon)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseBlockStatement(self: *Self) anyerror!ast.BlockStatement {
        var block = ast.BlockStatement.init(self.allocator, self.currentToken);
        self.nextToken();

        while (!self.currentTokenIs(token.TokenType.rbrace) and !self.currentTokenIs(token.TokenType.eof)) {
            const statement = try self.parseStatement();
            try block.statements.append(statement);
            self.nextToken();
        }
        return block;
    }

    fn parseExpression(self: *Self, precedence: u16) !ast.Expression {
        const prefixFn = self.prefixParseFns.get(self.currentToken.tokenType) orelse {
            return self.noPrefixParseFnError(self.currentToken.tokenType);
        };

        var leftExpr = try prefixFn(self);

        while (!self.peekTokenIs(token.TokenType.semicolon) and precedence < self.peekPrecedence()) {
            const infixFn = self.infixParseFns.get(self.peekToken.tokenType) orelse {
                return leftExpr;
            };

            self.nextToken();

            var leftExprPtr = utils.new(self.allocator, ast.Expression, leftExpr);
            leftExpr = try infixFn(self, leftExprPtr);
        }

        return leftExpr;
    }

    fn parseIdentifier(self: *Self) anyerror!ast.Expression {
        return ast.Expression{ .identifier = ast.Identifier{
            .token = self.currentToken,
            .value = self.currentToken.literal,
        } };
    }

    fn parseIntegerLiteral(self: *Self) anyerror!ast.Expression {
        var intLit = ast.IntegerLiteral{
            .token = self.currentToken,
            .value = undefined,
        };
        intLit.value = std.fmt.parseInt(i64, self.currentToken.literal, 10) catch {
            const msg = try fmt.allocPrint(self.allocator, "could not parse {s} as integer", .{self.currentToken.literal});
            try self.errors.append(msg);
            return ParserError.ConversionFailed;
        };

        return ast.Expression{
            .integerLiteral = intLit,
        };
    }

    fn parseBooleanExpression(self: *Self) anyerror!ast.Expression {
        return ast.Expression{
            .boolean = ast.Boolean{
                .token = self.currentToken,
                .value = self.currentTokenIs(token.TokenType.true_),
            },
        };
    }

    fn parseGroupedExpression(self: *Self) anyerror!ast.Expression {
        self.nextToken();
        const expr = self.parseExpression(@intFromEnum(OperatorPrecedence.lowest));
        try self.expectPeek(token.TokenType.rparen);

        return expr;
    }

    fn parseIfExpression(self: *Self) anyerror!ast.Expression {
        var ifExpr = ast.IfExpr{
            .token = self.currentToken,
            .condition = undefined,
            .consequence = undefined,
            .alternative = null,
        };

        try self.expectPeek(token.TokenType.lparen);

        self.nextToken();
        var exprOpt = try self.parseExpression(@intFromEnum(OperatorPrecedence.lowest));
        ifExpr.condition = utils.new(self.allocator, ast.Expression, exprOpt);

        try self.expectPeek(token.TokenType.rparen);

        try self.expectPeek(token.TokenType.lbrace);

        ifExpr.consequence = try self.parseBlockStatement();

        if (self.peekTokenIs(token.TokenType.else_)) {
            self.nextToken();
            try self.expectPeek(token.TokenType.lbrace);
            ifExpr.alternative = try self.parseBlockStatement();
        }

        return ast.Expression{
            .ifExpr = ifExpr,
        };
    }

    fn parseFuntionLiteral(self: *Self) anyerror!ast.Expression {
        var functionLit = ast.FunctionLiteral.init(self.allocator, self.currentToken);

        try self.expectPeek(token.TokenType.lparen);

        try self.parseFunctionParameters(&functionLit.parameters);

        try self.expectPeek(token.TokenType.lbrace);

        functionLit.body = try self.parseBlockStatement();

        return ast.Expression{
            .function = functionLit,
        };
    }

    fn parsePrefixExpression(self: *Self) anyerror!ast.Expression {
        var expr = ast.PrefixExpr{
            .token = self.currentToken,
            .operator = self.currentToken.literal,
            .right = undefined,
        };

        self.nextToken();

        const props = try self.parseExpression(@intFromEnum(OperatorPrecedence.prefix));
        expr.right = utils.new(self.allocator, ast.Expression, props);

        return ast.Expression{
            .prefixExpr = expr,
        };
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) anyerror!ast.Expression {
        var expr = ast.InfixExpr{
            .token = self.currentToken,
            .operator = self.currentToken.literal,
            .left = left,
            .right = undefined,
        };

        const precedence = self.currentPrecedence();
        self.nextToken();

        const props = try self.parseExpression(precedence);
        expr.right = utils.new(self.allocator, ast.Expression, props);

        return ast.Expression{
            .infixExpr = expr,
        };
    }

    fn parseCallExpression(self: *Self, function: *ast.Expression) anyerror!ast.Expression {
        var callExpr = ast.CallExpr.init(self.allocator, self.currentToken, function);
        try self.parseCallArguments(&callExpr.arguments);
        return ast.Expression{
            .callExpr = callExpr,
        };
    }

    fn parseFunctionParameters(self: *Self, params: *ArrayList(ast.Identifier)) anyerror!void {
        if (self.peekTokenIs(token.TokenType.rparen)) {
            self.nextToken();
            return;
        }

        self.nextToken();

        var ident = ast.Identifier{
            .token = self.currentToken,
            .value = self.currentToken.literal,
        };
        try params.append(ident);

        while (self.peekTokenIs(token.TokenType.comma)) {
            self.nextToken();
            self.nextToken();

            ident = ast.Identifier{
                .token = self.currentToken,
                .value = self.currentToken.literal,
            };
            try params.append(ident);
        }

        _ = try self.expectPeek(token.TokenType.rparen);
    }

    fn parseCallArguments(self: *Self, args: *ArrayList(ast.Expression)) anyerror!void {
        if (self.peekTokenIs(token.TokenType.rparen)) {
            self.nextToken();
            return;
        }

        self.nextToken();
        var expr = try self.parseExpression(@intFromEnum(OperatorPrecedence.lowest));
        try args.append(expr);

        while (self.peekTokenIs(token.TokenType.comma)) {
            self.nextToken();
            self.nextToken();
            expr = try self.parseExpression(@intFromEnum(OperatorPrecedence.lowest));
            try args.append(expr);
        }

        _ = try self.expectPeek(token.TokenType.rparen);
    }

    fn peekError(self: *Self, tokenType: token.TokenType) anyerror {
        const msg = try fmt.allocPrint(self.allocator, "expected next token to be {}, got {} instead", .{ tokenType, self.peekToken.tokenType });

        try self.errors.append(msg);
        return ParserError.UnexpectedToken;
    }

    fn noPrefixParseFnError(self: *Self, tokenType: token.TokenType) anyerror {
        const msg = try fmt.allocPrint(self.allocator, "no prefix parse function for {} found", .{tokenType});
        try self.errors.append(msg);
        return ParserError.UnexpectedToken;
    }

    fn currentTokenIs(self: *const Self, tokenType: token.TokenType) bool {
        return self.currentToken.tokenType == tokenType;
    }

    fn peekTokenIs(self: *const Self, tokenType: token.TokenType) bool {
        return self.peekToken.tokenType == tokenType;
    }

    fn expectPeek(self: *Self, tokenType: token.TokenType) !void {
        if (self.peekTokenIs(tokenType)) {
            self.nextToken();
            return;
        }
        return self.peekError(tokenType);
    }

    fn currentPrecedence(self: *Self) u16 {
        return tokenPrecedence(self.currentToken.tokenType);
    }

    fn peekPrecedence(self: *Self) u16 {
        return tokenPrecedence(self.peekToken.tokenType);
    }
};

fn freeExprPointer(allocator: Allocator, expr: ast.Expression) void {
    switch (expr) {
        .prefixExpr => |prefixExpr| {
            freeExprPointer(allocator, prefixExpr.right.*);
            allocator.destroy(prefixExpr.right);
        },
        .infixExpr => |infixExpr| {
            freeExprPointer(allocator, infixExpr.left.*);
            allocator.destroy(infixExpr.left);

            freeExprPointer(allocator, infixExpr.right.*);
            allocator.destroy(infixExpr.right);
        },
        .ifExpr => |ifExpr| {
            freeExprPointer(allocator, ifExpr.condition.*);
            allocator.destroy(ifExpr.condition);

            for (ifExpr.consequence.statements.items) |stmt| {
                switch (stmt) {
                    .exprStmt => |exprStmt| freeExprPointer(allocator, exprStmt.expr),
                    else => {},
                }
            }
            //TODO: why this won't work
            ifExpr.consequence.deinit();

            if (ifExpr.alternative) |alternative| {
                for (alternative.statements.items) |stmt| {
                    switch (stmt) {
                        .exprStmt => |exprStmt| freeExprPointer(allocator, exprStmt.expr),
                        else => {},
                    }
                }
                (&alternative).deinit();
            }
        },
        .function => |function| {
            for (function.body.statements.items) |stmt| {
                switch (stmt) {
                    .exprStmt => |exprStmt| freeExprPointer(allocator, exprStmt.expr),
                    else => {},
                }
            }
            (&function).deinit();
        },
        .callExpr => |callExpr| {
            freeExprPointer(allocator, callExpr.function.*);
            allocator.destroy(callExpr.function);
            for (callExpr.arguments.items) |arg| {
                freeExprPointer(allocator, arg);
            }

            (&callExpr).deinit();
        },
        else => {},
    }
}

fn tokenPrecedence(tokenType: token.TokenType) u16 {
    const value = switch (tokenType) {
        .eq => OperatorPrecedence.equals,
        .noteq => OperatorPrecedence.equals,
        .lt => OperatorPrecedence.lessGreater,
        .gt => OperatorPrecedence.lessGreater,
        .plus => OperatorPrecedence.sum,
        .minus => OperatorPrecedence.sum,
        .asterisk => OperatorPrecedence.product,
        .slash => OperatorPrecedence.product,
        .lparen => OperatorPrecedence.call,
        else => OperatorPrecedence.lowest,
    };
    return @intFromEnum(value);
}

test "TestLetStatements" {
    const allocator = testing.allocator;
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();

    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 3), program.statements.items.len);

    const TestCase = struct { []const u8, i64 };
    const tests = [_]TestCase{
        .{ "x", 5 },
        .{ "y", 10 },
        .{ "foobar", 838383 },
    };

    for (tests, 0..) |testCase, i| {
        const statement = program.statements.items[i];

        try testing.expectEqualStrings("let", statement.tokenLiteral());
        try testing.expectEqual(ast.Statement.letStmt, statement);

        const letSmt = statement.letStmt;
        try testing.expectEqualStrings(testCase[0], letSmt.name.value);
        try testing.expectEqualStrings(testCase[0], letSmt.name.tokenLiteral());

        const intLit = letSmt.value.integerLiteral;
        try testing.expectEqual(testCase[1], intLit.value);
    }
}

test "TestReturnStatements" {
    const allocator = testing.allocator;
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());

    try testing.expectEqual(@as(usize, 3), program.statements.items.len);
    const expectedIntLit = [_]i64{ 5, 10, 993322 };
    for (0..3) |i| {
        const statement = program.statements.items[i];
        try testing.expectEqualStrings("return", statement.tokenLiteral());
        try testing.expectEqual(ast.Statement.returnStmt, statement);

        const returnSmt = statement.returnStmt;
        try testing.expectEqualStrings("return", returnSmt.tokenLiteral());

        const intLit = returnSmt.value.integerLiteral;
        try testing.expectEqual(expectedIntLit[i], intLit.value);
    }
}

test "TestIndentiferExpression" {
    const allocator = testing.allocator;
    const input = "foobar;";

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const statement = program.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, statement);

    const exprStmt = statement.exprStmt;
    try testing.expectEqualStrings("foobar", exprStmt.tokenLiteral());
}

test "TestBooleanExpression" {
    const allocator = testing.allocator;
    const input =
        \\true;
        \\false;
    ;

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 2), program.statements.items.len);

    const expectations = [2]bool{ true, false };
    for (expectations, 0..) |expected, i| {
        const statement = program.statements.items[i];
        try testing.expectEqual(ast.Statement.exprStmt, statement);

        const boolLit = statement.exprStmt.expr.boolean;
        try testing.expectEqual(expected, boolLit.value);
    }
}

test "TestIntegerLiteralExpression" {
    const allocator = testing.allocator;
    const input = "5;";

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const statement = program.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, statement);

    const intLit = statement.exprStmt.expr.integerLiteral;
    try testing.expectEqual(@as(i64, 5), intLit.value);
}

test "TestPrefixExpression" {
    const allocator = testing.allocator;
    const TestCase = struct { []const u8, []const u8, i64 };
    const tests = [_]TestCase{
        .{ "!5;", "!", 5 },
        .{ "-15;", "-", 15 },
    };

    for (tests) |testCase| {
        var lexr = lexer.Lexer.new(testCase[0]);
        var parser = Parser.new(allocator, &lexr);
        defer parser.deinit();

        var program = try parser.parseProgram();
        try checkErrors(parser.getErrors());
        try testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const statement = program.statements.items[0];
        try testing.expectEqual(ast.Statement.exprStmt, statement);

        const prefixExpr = statement.exprStmt.expr.prefixExpr;

        try testing.expectEqualStrings(testCase[1], prefixExpr.operator);

        const intLit = prefixExpr.right.integerLiteral;
        try testing.expectEqual(@as(i64, testCase[2]), intLit.value);
    }
}

test "TestParsingInfixExpressions" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        i64,
        []const u8,
        i64,
    };
    const tests = [_]TestCase{
        .{ "5 + 5;", 5, "+", 5 },
        .{ "5 - 5;", 5, "-", 5 },
        .{ "5 * 5;", 5, "*", 5 },
        .{ "5 / 5;", 5, "/", 5 },
        .{ "5 > 5;", 5, ">", 5 },
        .{ "5 < 5;", 5, "<", 5 },
        .{ "5 == 5;", 5, "==", 5 },
        .{ "5 != 5;", 5, "!=", 5 },
    };

    for (tests) |testCase| {
        var lexr = lexer.Lexer.new(testCase[0]);
        var parser = Parser.new(allocator, &lexr);
        defer parser.deinit();

        var program = try parser.parseProgram();
        try checkErrors(parser.getErrors());
        try testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const statement = program.statements.items[0];
        try testing.expectEqual(ast.Statement.exprStmt, statement);

        const infixExpr = statement.exprStmt.expr.infixExpr;

        const leftIntLit = infixExpr.left.integerLiteral;
        try testing.expectEqual(@as(i64, testCase[1]), leftIntLit.value);

        try testing.expectEqualStrings(testCase[2], infixExpr.operator);

        const rightIntLit = infixExpr.right.integerLiteral;
        try testing.expectEqual(@as(i64, testCase[3]), rightIntLit.value);
    }
}

test "TestParsingInfixExpressionsWithBool" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        bool,
        []const u8,
        bool,
    };
    const tests = [_]TestCase{
        .{ "true == true", true, "==", true },
        .{ "true != false", true, "!=", false },
        .{ "false == false", false, "==", false },
    };

    for (tests) |testCase| {
        var lexr = lexer.Lexer.new(testCase[0]);
        var parser = Parser.new(allocator, &lexr);
        defer parser.deinit();

        var program = try parser.parseProgram();
        try checkErrors(parser.getErrors());
        try testing.expectEqual(@as(usize, 1), program.statements.items.len);

        const statement = program.statements.items[0];
        try testing.expectEqual(ast.Statement.exprStmt, statement);

        const infixExpr = statement.exprStmt.expr.infixExpr;

        const leftBoolLit = infixExpr.left.boolean;
        try testing.expectEqual(@as(bool, testCase[1]), leftBoolLit.value);

        try testing.expectEqualStrings(testCase[2], infixExpr.operator);

        const rightBoolLit = infixExpr.right.boolean;
        try testing.expectEqual(@as(bool, testCase[3]), rightBoolLit.value);
    }
}

test "TestOperatorPrecedenceParsing" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        []const u8,
    };
    const tests = [_]TestCase{
        .{ "-a * b", "((-a) * b)" },
        .{ "!-a", "(!(-a))" },
        .{ "a + b + c", "((a + b) + c)" },
        .{ "a + b - c", "((a + b) - c)" },
        .{ "a * b * c", "((a * b) * c)" },
        .{ "a * b / c", "((a * b) / c)" },
        .{ "a + b / c", "(a + (b / c))" },
        .{ "a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)" },
        .{ "3 + 4; -5 * 5", "(3 + 4)((-5) * 5)" },
        .{ "5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))" },
        .{ "5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))" },
        .{ "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ "3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" },
        .{ "true", "true" },
        .{ "false", "false" },
        .{ "3 > 5 == false", "((3 > 5) == false)" },
        .{ "3 < 5 == true", "((3 < 5) == true)" },
        .{ "1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)" },
        .{ "(5 + 5) * 2", "((5 + 5) * 2)" },
        .{ "2 / (5 + 5)", "(2 / (5 + 5))" },
        .{ "-(5 + 5)", "(-(5 + 5))" },
        .{ "!(true == true)", "(!(true == true))" },
        .{ "a + add(b * c) + d", "((a + add((b * c))) + d)" },
        .{ "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))" },
        .{ "add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))" },
    };

    for (tests) |testCase| {
        var lexr = lexer.Lexer.new(testCase[0]);
        var parser = Parser.new(allocator, &lexr);
        defer parser.deinit();

        var program = try parser.parseProgram();
        try checkErrors(parser.getErrors());

        var buffer = ArrayList(u8).init(allocator);
        defer buffer.deinit();
        try program.toString(buffer.writer());
        try testing.expectEqualStrings(testCase[1], buffer.items);
    }
}

test "TestIfExpression" {
    const allocator = testing.allocator;
    const input = "if (x < y) { x }";

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const statement = program.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, statement);

    const ifExpr = statement.exprStmt.expr.ifExpr;
    const conditionExpr = ifExpr.condition.infixExpr;

    const leftIdent = conditionExpr.left.identifier;
    try testing.expectEqualStrings("x", leftIdent.value);

    try testing.expectEqualStrings("<", conditionExpr.operator);

    const rightIdent = conditionExpr.right.identifier;
    try testing.expectEqualStrings("y", rightIdent.value);

    try testing.expectEqual(@as(usize, 1), ifExpr.consequence.statements.items.len);
    const consequenceStatement = ifExpr.consequence.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, consequenceStatement);
    try testing.expectEqualStrings("x", consequenceStatement.tokenLiteral());

    try testing.expect(ifExpr.alternative == null);
}

test "TestIfElseExpression" {
    const allocator = testing.allocator;
    const input = "if (x < y) { x } else { y }";

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const statement = program.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, statement);

    const ifExpr = statement.exprStmt.expr.ifExpr;
    const conditionExpr = ifExpr.condition.infixExpr;

    const leftIdent = conditionExpr.left.identifier;
    try testing.expectEqualStrings("x", leftIdent.value);

    try testing.expectEqualStrings("<", conditionExpr.operator);

    const rightIdent = conditionExpr.right.identifier;
    try testing.expectEqualStrings("y", rightIdent.value);

    try testing.expectEqual(@as(usize, 1), ifExpr.consequence.statements.items.len);
    const consequenceStatement = ifExpr.consequence.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, consequenceStatement);
    try testing.expectEqualStrings("x", consequenceStatement.tokenLiteral());

    if (ifExpr.alternative) |alternative| {
        try testing.expectEqual(@as(usize, 1), alternative.statements.items.len);
        const alternativeStatement = alternative.statements.items[0];
        try testing.expectEqual(ast.Statement.exprStmt, alternativeStatement);
        try testing.expectEqualStrings("y", alternativeStatement.tokenLiteral());
    }
}

test "TestFuntionLiteral" {
    const allocator = testing.allocator;
    const input = "fn(x, y) { x + y; }";

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const statement = program.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, statement);

    const functionLit = statement.exprStmt.expr.function;
    const params = functionLit.parameters;
    try testing.expectEqual(@as(usize, 2), params.items.len);
    try testing.expectEqualStrings("x", params.items[0].value);
    try testing.expectEqualStrings("y", params.items[1].value);

    try testing.expectEqual(@as(usize, 1), functionLit.body.statements.items.len);
    const bodyStmt = functionLit.body.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, bodyStmt);
    try testing.expectEqualStrings("x", bodyStmt.tokenLiteral());
}

test "TestFunctionParameterParsing" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        usize,
    };
    const tests = [_]TestCase{
        .{ "fn() {};", 0 },
        .{ "fn(x) {};", 1 },
        .{ "fn(x, y, z) {};", 3 },
    };

    for (tests) |testCase| {
        var lexr = lexer.Lexer.new(testCase[0]);
        var parser = Parser.new(allocator, &lexr);
        defer parser.deinit();

        var program = try parser.parseProgram();
        try checkErrors(parser.getErrors());

        const statement = program.statements.items[0];
        try testing.expectEqual(ast.Statement.exprStmt, statement);

        const functionLit = statement.exprStmt.expr.function;
        try testing.expectEqual(testCase[1], functionLit.parameters.items.len);
    }
}

test "TestCallExpressionParsing" {
    const allocator = testing.allocator;
    const input = "add(1, 2 * 3, 4 + 5);";

    var lexr = lexer.Lexer.new(input);
    var parser = Parser.new(allocator, &lexr);
    defer parser.deinit();

    var program = try parser.parseProgram();
    try checkErrors(parser.getErrors());
    try testing.expectEqual(@as(usize, 1), program.statements.items.len);

    const statement = program.statements.items[0];
    try testing.expectEqual(ast.Statement.exprStmt, statement);

    const callExpr = statement.exprStmt.expr.callExpr;
    try testing.expectEqual(@as(usize, 3), callExpr.arguments.items.len);
    try testing.expectEqualStrings("add", callExpr.function.tokenLiteral());
}

// ----------------
fn testLetStatement(node: ast.Node, name: []const u8) !void {
    try testing.expectEqualStrings("let", node.tokenLiteral());
    // try testing.expectEqual(ast.Statement.letStmt, node);
    _ = name;

    // const letStatement = node.statement.letStmt;
    // try testing.expectEqualStrings(name, letStatement.name.value);
    // try testing.expectEqualStrings(name, letStatement.name.tokenLiteral());
}

fn checkErrors(errors: [][]const u8) !void {
    for (errors) |err| {
        std.debug.print("-> {s}\n", .{err});
    }
    try testing.expectEqual(@as(usize, 0), errors.len);
}

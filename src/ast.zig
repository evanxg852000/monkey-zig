const std = @import("std");

const token = @import("./token.zig");
const utils = @import("./utils.zig");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const testing = std.testing;
const Token = token.Token;
const TokenType = token.TokenType;

pub const Program = struct {
    const Self = @This();

    statements: ArrayList(Statement),

    pub fn init(allocator: Allocator) Self {
        return Self{
            .statements = ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // for (self.nodes.items) |*node| {
        //     node.deinit();
        // }
        self.statements.deinit();
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        if (self.statements.len > 1) {
            return self.statements[0].tokenLiteral();
        }
        return "";
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        for (self.statements.items) |statement| {
            try statement.toString(writer);
        }
    }
};

pub const Statement = union(enum) {
    const Self = @This();

    letStmt: LetStatement,
    returnStmt: ReturnStatement,
    exprStmt: ExpressionStatement,

    pub fn tokenLiteral(self: *const Self) []const u8 {
        //return self.tokenLiteral();
        return switch (self.*) {
            .letStmt => |letStmt| letStmt.tokenLiteral(),
            .returnStmt => |returnStmt| returnStmt.tokenLiteral(),
            .exprStmt => |exprStmt| exprStmt.tokenLiteral(),
        };
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        return switch (self.*) {
            .letStmt => |letStmt| letStmt.toString(writer),
            .returnStmt => |returnStmt| returnStmt.toString(writer),
            .exprStmt => |exprStmt| exprStmt.toString(writer),
        };
    }
};

pub const Expression = union(enum) {
    const Self = @This();

    identifier: Identifier,
    integerLiteral: IntegerLiteral,
    boolean: Boolean,
    prefixExpr: PrefixExpr,
    infixExpr: InfixExpr,
    ifExpr: IfExpr,
    function: FunctionLiteral,
    callExpr: CallExpr,

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return switch (self.*) {
            .identifier => |ident| ident.tokenLiteral(),
            .integerLiteral => |intLit| intLit.tokenLiteral(),
            .boolean => |boolLit| boolLit.tokenLiteral(),
            .prefixExpr => |prefixExpr| prefixExpr.tokenLiteral(),
            .infixExpr => |infixExpr| infixExpr.tokenLiteral(),
            .ifExpr => |ifExpr| ifExpr.tokenLiteral(),
            .function => |function| function.tokenLiteral(),
            .callExpr => |callExpr| callExpr.tokenLiteral(),
        };
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        return switch (self.*) {
            .identifier => |ident| ident.toString(writer),
            .boolean => |b| b.toString(writer),
            .integerLiteral => |intLit| intLit.toString(writer),
            .prefixExpr => |prefixExpr| prefixExpr.toString(writer),
            .infixExpr => |infixExpr| infixExpr.toString(writer),
            .ifExpr => |ifExpr| ifExpr.toString(writer),
            .function => |function| function.toString(writer),
            .callExpr => |callExpr| callExpr.toString(writer),
        };
    }
};

pub const Identifier = struct {
    const Self = @This();

    token: Token,
    value: []const u8,

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        _ = try writer.write(self.value);
    }
};

pub const IntegerLiteral = struct {
    const Self = @This();

    token: Token,
    value: i64,

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        _ = try writer.write(self.token.literal);
    }
};

pub const Boolean = struct {
    const Self = @This();

    token: Token,
    value: bool,

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        _ = try writer.write(self.token.literal);
    }
};

pub const IfExpr = struct {
    const Self = @This();

    token: Token,
    condition: *Expression,
    consequence: BlockStatement,
    alternative: ?BlockStatement,

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        _ = try writer.write("if");
        try self.condition.toString(writer);
        _ = try writer.write(" ");
        try self.consequence.toString(writer);
        if (self.alternative) |alternative| {
            _ = try writer.write("else ");
            try alternative.toString(writer);
        }
    }
};

pub const CallExpr = struct {
    const Self = @This();

    token: Token,
    function: *Expression,
    arguments: ArrayList(Expression),

    pub fn init(allocator: Allocator, tkn: Token, function: *Expression) Self {
        return Self{
            .token = tkn,
            .function = function,
            .arguments = ArrayList(Expression).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.arguments.deinit();
    }

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        try self.function.toString(writer);
        _ = try writer.write("(");
        const size = self.arguments.items.len;
        for (self.arguments.items, 0..) |arg, i| {
            try arg.toString(writer);
            if (i < size - 1) {
                _ = try writer.write(", ");
            }
        }
        _ = try writer.write(")");
    }
};

pub const PrefixExpr = struct {
    const Self = @This();

    token: Token,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) anyerror!void {
        _ = try writer.write("(");
        try writer.print("{s}", .{self.operator});
        try self.right.toString(writer);
        _ = try writer.write(")");
    }
};

pub const InfixExpr = struct {
    const Self = @This();

    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) anyerror!void {
        _ = try writer.write("(");
        try self.left.toString(writer);
        try writer.print(" {s} ", .{self.operator});
        try self.right.toString(writer);
        _ = try writer.write(")");
    }
};

pub const LetStatement = struct {
    const Self = @This();

    token: Token,
    name: Identifier,
    value: Expression,

    pub fn statementNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        try writer.print("{s} ", .{self.tokenLiteral()});
        try self.name.toString(writer);
        _ = try writer.write(" = ");
        try self.value.toString(writer);
        _ = try writer.write(";");
    }
};

pub const ReturnStatement = struct {
    const Self = @This();

    token: Token,
    value: Expression,

    pub fn statementNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        try writer.print("{s} ", .{self.tokenLiteral()});
        try self.value.toString(writer);
        _ = try writer.write(";");
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();

    token: Token,
    expr: Expression,

    pub fn statementNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        try self.expr.toString(writer);
    }
};

pub const BlockStatement = struct {
    const Self = @This();

    token: Token,
    statements: ArrayList(Statement),

    pub fn init(allocator: Allocator, tkn: Token) Self {
        return Self{
            .token = tkn,
            .statements = ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.statements.deinit();
    }

    pub fn statementNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        for (self.statements.items) |statement| {
            try statement.toString(writer);
        }
    }
};

pub const FunctionLiteral = struct {
    const Self = @This();

    token: Token,
    parameters: ArrayList(Identifier),
    body: BlockStatement,

    pub fn init(allocator: Allocator, tkn: Token) Self {
        return Self{
            .token = tkn,
            .parameters = ArrayList(Identifier).init(allocator),
            .body = undefined,
        };
    }

    pub fn deinit(self: Self) void {
        self.parameters.deinit();
        self.body.deinit();
    }

    pub fn expressionNode(self: *Self) void {
        _ = self;
    }

    pub fn tokenLiteral(self: *const Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: *const Self, writer: anytype) !void {
        _ = try writer.write(self.tokenLiteral());
        _ = try writer.write("(");
        const size = self.parameters.items.len;
        for (self.parameters.items, 0..) |param, i| {
            try param.toString(writer);
            if (i < size - 1) {
                _ = try writer.write(", ");
            }
        }
        _ = try writer.write(") ");
        try self.body.toString(writer);
    }
};

test "TestAstToString" {
    const allocator = testing.allocator;
    var statements = ArrayList(Statement).init(allocator);
    try statements.append(Statement{
        .letStmt = LetStatement{
            .token = token.Token.new(token.TokenType.let, "let"),
            .name = Identifier{
                .token = token.Token.new(token.TokenType.ident, "myVar"),
                .value = "myVar",
            },
            .value = Expression{
                .identifier = Identifier{
                    .token = token.Token.new(token.TokenType.ident, "anotherVar"),
                    .value = "anotherVar",
                },
            },
        },
    });

    var program = Program{
        .statements = statements,
    };
    defer program.deinit();

    var buffer = ArrayList(u8).init(allocator);
    defer buffer.deinit();
    try program.toString(buffer.writer());
    try testing.expectEqualStrings("let myVar = anotherVar;", buffer.items);
}

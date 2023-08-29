const std = @import("std");
const testing = std.testing;
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const ast = @import("./ast.zig");
const object = @import("./object.zig");

const Allocator = std.mem.Allocator;

pub fn eval_program(program: *ast.Program) !object.Object {
    var return_value = object.Object.nil();
    const count = program.statements.items.len;
    for (program.statements.items, 0..) |*stmt, i| {
        if (i == count - 1) { // return value of last statement
            return_value = try eval_statement(stmt);
            continue;
        }
        _ = try eval_statement(stmt);
    }

    return return_value;
}

fn eval_statement(stmt: *ast.Statement) !object.Object {
    return switch (stmt.*) {
        .letStmt => |*letStmt| eval_let_stmt(letStmt),
        .returnStmt => |*returnStmt| eval_return_stmt(returnStmt),
        .exprStmt => |*exprStmt| eval_expr(&exprStmt.expr),
    };
}

fn eval_let_stmt(letStmt: *ast.LetStatement) !object.Object {
    _ = letStmt;
    return object.Object.nil();
}

fn eval_return_stmt(returnStmt: *ast.ReturnStatement) !object.Object {
    _ = returnStmt;
    return object.Object.nil();
}

fn eval_expr(expr: *ast.Expression) !object.Object {
    return switch (expr.*) {
        .identifier => object.Object.nil(),
        .integerLiteral => |v| object.Object.integer(v.value),
        .boolean => |v| object.Object.boolean(v.value),
        .prefixExpr => |v| {
            const right = try eval_expr(v.right);
            return switch (v.token.tokenType) {
                .bang => try eval_prefix_bang_expr(right),
                .minus => try eval_prefix_minus_expr(right),
                else => object.Object.nil(),
            };
        },
        .infixExpr => |v| {
            const left = try eval_expr(v.left);
            const right = try eval_expr(v.right);
            if (left.isOtype(object.OType.integer) and right.isOtype(object.OType.integer)) {
                return eval_interger_infix_expr(v.operator, left.integer.value, right.integer.value);
            }

            if (left.isOtype(object.OType.boolean) and right.isOtype(object.OType.boolean)) {
                if (std.mem.eql(u8, v.operator, "==")) {
                    return object.Object.boolean(left.boolean.value == right.boolean.value);
                }

                if (std.mem.eql(u8, v.operator, "!=")) {
                    return object.Object.boolean(left.boolean.value != right.boolean.value);
                }
            }

            return object.Object.nil();
        },
        .ifExpr => object.Object.nil(),
        .function => object.Object.nil(),
        .callExpr => object.Object.nil(),
    };
}

fn eval_prefix_bang_expr(right: object.Object) !object.Object {
    return switch (right) {
        .boolean => |v| object.Object.boolean(!v.value),
        .nil => object.Object.boolean(true),
        else => object.Object.boolean(false),
    };
}

fn eval_prefix_minus_expr(right: object.Object) !object.Object {
    return switch (right) {
        .integer => |v| object.Object.integer(-v.value),
        else => object.Object.nil(),
    };
}

fn eval_interger_infix_expr(operator: []const u8, left: i64, right: i64) !object.Object {
    return switch (operator[0]) {
        '+' => object.Object.integer(left + right),
        '-' => object.Object.integer(left - right),
        '*' => object.Object.integer(left * right),
        '/' => object.Object.integer(@divTrunc(left, right)),
        '<' => object.Object.boolean(left < right),
        '>' => object.Object.boolean(left > right),
        else => blk: {
            if (std.mem.eql(u8, operator, "==")) {
                break :blk object.Object.boolean(left == right);
            }

            if (std.mem.eql(u8, operator, "!=")) {
                break :blk object.Object.boolean(left != right);
            }

            break :blk object.Object.nil();
        },
    };
}

test "TestEvalIntegerExpression" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        i64,
    };
    const tests = [_]TestCase{
        .{ "5", 5 },
        .{ "10;", 10 },
        .{ "5", 5 },
        .{ "10", 10 },
        .{ "-5", -5 },
        .{ "-10", -10 },
        .{ "5 + 5 + 5 + 5 - 10", 10 },
        .{ "2 * 2 * 2 * 2 * 2", 32 },
        .{ "-50 + 100 + -50", 0 },
        .{ "5 * 2 + 10", 20 },
        .{ "5 + 2 * 10", 25 },
        .{ "20 + 2 * -10", 0 },
        .{ "50 / 2 * 2 + 10", 60 },
        .{ "2 * (5 + 10)", 30 },
        .{ "3 * 3 * 3 + 10", 37 },
        .{ "3 * (3 * 3) + 10", 37 },
        .{ "(5 + 10 * 2 + 15 / 3) * 2 + -10", 50 },
    };

    for (tests) |testCase| {
        var obj = try testEval(allocator, testCase[0]);
        const intObject = obj.integer;
        try testing.expectEqual(@as(i64, testCase[1]), intObject.value);
    }
}

test "TestEvalBooleanExpression" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        bool,
    };
    const tests = [_]TestCase{
        .{ "true", true },
        .{ "false;", false },
        .{ "1 < 2", true },
        .{ "1 > 2", false },
        .{ "1 < 1", false },
        .{ "1 > 1", false },
        .{ "1 == 1", true },
        .{ "1 != 1", false },
        .{ "1 == 2", false },
        .{ "1 != 2", true },
        .{ "true == true", true },
        .{ "false == false", true },
        .{ "true == false", false },
        .{ "true != false", true },
        .{ "false != true", true },
        .{ "(1 < 2) == true", true },
        .{ "(1 < 2) == false", false },
        .{ "(1 > 2) == true", false },
        .{ "(1 > 2) == false", true },
    };

    for (tests) |testCase| {
        var obj = try testEval(allocator, testCase[0]);
        const boolObject = obj.boolean;
        try testing.expectEqual(@as(bool, testCase[1]), boolObject.value);
    }
}

test "TestBangOperator" {
    const allocator = testing.allocator;
    const TestCase = struct {
        []const u8,
        bool,
    };
    const tests = [_]TestCase{
        .{ "!true", false },
        .{ "!false", true },
        .{ "!5", false },
        .{ "!!true", true },
        .{ "!!false", false },
        .{ "!!5", true },
    };

    for (tests) |testCase| {
        var obj = try testEval(allocator, testCase[0]);
        const boolObject = obj.boolean;
        try testing.expectEqual(@as(bool, testCase[1]), boolObject.value);
    }
}

fn testEval(allocator: Allocator, input: []const u8) !object.Object {
    var lexr = lexer.Lexer.new(input);
    var psr = parser.Parser.new(allocator, &lexr);
    defer psr.deinit();

    // program is owned by parser
    var program = try psr.parseProgram();
    try parser.checkErrors(psr.getErrors());

    return eval_program(program);
}

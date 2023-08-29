const std = @import("std");
const repl = @import("./repl.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Hello! this is the Monkey programming language!\n", .{});
    try stdout.print("Feel free to type in commands\n", .{});
    try repl.start(allocator, stdin, stdout);
}

test {
    _ = @import("./token.zig");
    _ = @import("./lexer.zig");
    _ = @import("./parser.zig");
    _ = @import("./ast.zig");
    _ = @import("./eval.zig");
    _ = @import("./repl.zig");
}

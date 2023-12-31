const std = @import("std");
const lexer = @import("./lexer.zig");
const token = @import("./token.zig");
const parser = @import("./parser.zig");
const eval = @import("./eval.zig");

const Allocator = std.mem.Allocator;

const prompt = ">> ";

pub fn start(allocator: Allocator, reader: anytype, writer: anytype) !void {
    var buf: [1024]u8 = undefined;
    while (true) {
        try writer.print("{s}", .{prompt});
        if (try reader.readUntilDelimiterOrEof(buf[0..], '\n')) |input| {
            if (isExitCommand(input)) {
                break;
            }
            var lxr = lexer.Lexer.new(input);
            var psr = parser.Parser.new(allocator, &lxr);
            defer psr.deinit();

            const program = try psr.parseProgram();
            const errors = psr.getErrors();
            if (errors.len > 0) {
                for (errors) |err| {
                    std.debug.print("error: {s}\n", .{err});
                }
                continue;
            }

            const result = try eval.eval_program(program);
            try result.inspect(writer);
            _ = try writer.write("\n");
        } else {
            break;
        }
    }
}

fn isExitCommand(input: []const u8) bool {
    return std.mem.eql(u8, input, ".exit");
}

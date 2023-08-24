const std = @import("std");

// const FooError = error{Bar};

// fn foo() void {
//     std.debug.print("FOOOOO\n", .{});
// }

// fn f(i: u32) !void {
//     if (i != 42) return FooError.Bar;
// }

// test "Foo" {
//     defer foo();
//     try f(4);
// }

const Foo = struct {
    const Self = @This();

    x: u32,

    pub fn init() Self {
        var f = Self{ .x = 12 };
        const b = f.bar;
        _ = b() or unreachable;
        return f;
    }

    fn bar(self: *Self) !void {
        _ = self;
    }
};

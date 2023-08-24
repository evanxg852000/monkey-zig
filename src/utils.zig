const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn isLetter(char: u8) bool {
    return ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or char == '_';
}

pub fn isDigit(char: u8) bool {
    return '0' <= char and char <= '9';
}

pub fn new(allocator: Allocator, comptime T: type, props: anytype) *T {
    // simplifying eror handling
    const ptr = allocator.create(T) catch unreachable;
    // errdefer allocator.destroy(node);
    ptr.* = props;
    return ptr;
}

test "test new" {
    const alloc = std.testing.allocator;
    const Point = struct {
        x: u32,
        y: u32,
    };

    const p1 = new(alloc, Point, .{ .x = 32, .y = 12 });
    defer alloc.destroy(p1);

    try std.testing.expectEqual(@as(u32, 32), p1.x);
    try std.testing.expectEqual(@as(u32, 12), p1.y);
}

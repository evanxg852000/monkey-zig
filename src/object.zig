const std = @import("std");

pub const Interger = struct {
    const Self = @This();

    value: i64,

    pub fn inspect(self: *const Self, writer: anytype) !void {
        try writer.print("{d}", .{self.value});
    }
};

pub const Boolean = struct {
    const Self = @This();

    value: bool,

    pub fn inspect(self: *const Self, writer: anytype) !void {
        if (self.value) {
            _ = try writer.write("true");
        } else {
            _ = try writer.write("false");
        }
    }
};

pub const Nil = struct {
    const Self = @This();

    pub fn inspect(self: *const Self, writer: anytype) !void {
        _ = self;
        _ = try writer.write("nil");
    }
};

const TrueObject = Boolean{ .value = true };
const FalseObject = Boolean{ .value = false };
const NilObject = Nil{};

pub const OType = enum {
    integer,
    boolean,
    nil,
};

pub const Object = union(OType) {
    const Self = @This();

    integer: Interger,
    boolean: Boolean,
    nil: Nil,

    pub fn otype(self: *const Self) OType {
        return switch (self.*) {
            .integer => OType.integer,
            .boolean => OType.boolean,
            .nil => OType.nil,
        };
    }

    pub fn isOtype(self: *const Self, ttype: OType) bool {
        return self.otype() == ttype;
    }

    pub fn inspect(self: *const Self, writer: anytype) !void {
        return switch (self.*) {
            .integer => |*v| v.inspect(writer),
            .boolean => |*v| v.inspect(writer),
            .nil => |*v| v.inspect(writer),
        };
    }

    pub fn nil() Self {
        return Self{
            .nil = NilObject,
        };
    }

    pub fn integer(v: i64) Self {
        return Self{
            .integer = Interger{ .value = v },
        };
    }

    pub fn boolean(v: bool) Self {
        const boolVal = if (v) TrueObject else FalseObject;
        return Self{
            .boolean = boolVal,
        };
    }
};

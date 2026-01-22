const std = @import("std");
const mem = std.mem;

const basic_types = @import("basic_types.zig");
const Square = basic_types.Square;

pub fn sqToIndex(sq: []const u8) ?Square {
    if (mem.eql(u8, sq, "-")) return null;
    const rank = sq[1] - '1';
    const file = sq[0] - 'a';
    return rank * 8 + file;
}

pub fn indexToSq(index: ?Square) []const u8 {
    if (index) |idx| {
        const rank = idx / 8;
        const file = idx % 8;

        var sq: [2]u8 = undefined;
        sq[0] = 'a' + file;
        sq[1] = '1' + rank;

        return sq[0..];
    }

    return "-";
}

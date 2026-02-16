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

pub fn indexToSq(index: ?Square) [2]u8 {
    if (index) |idx| {
        const rank = idx / 8;
        const file = idx % 8;
        return .{ 'a' + file, '1' + rank };
    }
    return .{ '-', 0 };
}

pub fn parseMove(move: []const u8) basic_types.Move {
    const from_sq = sqToIndex(move[0..2]).?;
    const to_sq = sqToIndex(move[2..4]).?;

    var promotion: ?basic_types.PieceType = null;
    if (move.len == 5) {
        switch (move[4]) {
            'q' => promotion = .Queen,
            'r' => promotion = .Rook,
            'b' => promotion = .Bishop,
            'n' => promotion = .Knight,
            else => unreachable,
        }
    }

    return basic_types.Move{
        .from = from_sq,
        .to = to_sq,
        .promotion = promotion,
    };
}

pub fn moveToStr(move: basic_types.Move) [5]u8 {
    const from_sq = indexToSq(move.from);
    const to_sq = indexToSq(move.to);

    var buf: [5]u8 = .{
        from_sq[0], from_sq[1],
        to_sq[0],   to_sq[1],
        0,
    };

    if (move.promotion) |promo| {
        buf[4] = switch (promo) {
            .Queen => 'q',
            .Rook => 'r',
            .Bishop => 'b',
            .Knight => 'n',
            else => unreachable,
        };
    }

    return buf;
}

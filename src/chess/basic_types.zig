const std = @import("std");
const bit_set = std.bit_set;

pub const Bitboard = bit_set.IntegerBitSet(64);
pub const Square = u8;

pub const Color = enum {
    White,
    Black,
};

pub const PieceType = enum {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
};

pub const Piece = struct {
    color: Color,
    piece_type: PieceType,
};

pub const Direction = enum(i8) {
    North = 8,
    East = 1,
    South = -8,
    West = -1,
    NorthEast = 9,
    SouthEast = -7,
    SouthWest = -9,
    NorthWest = 7,
};

pub const Move = struct {
    from: Square,
    to: Square,
    promotion: ?PieceType,
};

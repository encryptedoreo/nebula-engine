const std = @import("std");
const bit_set = std.bit_set;
const ascii = std.ascii;
const fmt = std.fmt;

const basic_types = @import("basic_types.zig");
const Bitboard = basic_types.Bitboard;
const Square = basic_types.Square;
const Color = basic_types.Color;
const PieceType = basic_types.PieceType;
const Move = basic_types.Move;
const Piece = basic_types.Piece;

const errors = @import("errors.zig");
const ChessError = errors.ChessError;

const utils = @import("utils.zig");
const sqToIndex = utils.sqToIndex;

const Self = @This();

const zb_hash_const = blk: {
    @setEvalBranchQuota(4294967295);
    var arr: [639]u64 = undefined;
    var rand = std.Random.DefaultPrng.init(0x9E3779B97F4A7C15);
    for (0..639) |i| {
        arr[i] = rand.next();
    }
    break :blk arr;
};

pieces: [6]Bitboard,
colours: [2]Bitboard,
side_to_move: Color,
castle_rights: [2]bit_set.IntegerBitSet(8),
ep_square: ?Square,
halfmove_clock: u8,
fullmove_number: u16,
hist: [6]u64,
hash: u64,
attack_bitboards: [64]Bitboard,

pub fn startPosition() Self {
    const PAWN_MASK = 0x00FF00000000FF00;
    const KNIGHT_MASK = 0x4200000000000042;
    const BISHOP_MASK = 0x2400000000000024;
    const ROOK_MASK = 0x8100000000000081;
    const QUEEN_MASK = 0x1000000000000010;
    const KING_MASK = 0x0800000000000008;

    const WHITE_MASK = 0x000000000000FFFF;
    const BLACK_MASK = 0xFFFF000000000000;

    const castle_rights: bit_set.IntegerBitSet(8) = .{ .mask = 0b10000001 };

    return Self{
        .pieces = .{
            .{ .mask = PAWN_MASK },
            .{ .mask = KNIGHT_MASK },
            .{ .mask = BISHOP_MASK },
            .{ .mask = ROOK_MASK },
            .{ .mask = QUEEN_MASK },
            .{ .mask = KING_MASK },
        },
        .colours = .{
            .{ .mask = WHITE_MASK },
            .{ .mask = BLACK_MASK },
        },
        .side_to_move = Color.White,
        .castle_rights = @splat(castle_rights),
        .ep_square = null,
        .halfmove_clock = 0,
        .fullmove_number = 0,
        .hist = @splat(0),
        .hash = 11172781560932740185,
        .attack_bitboards = @splat(.{ .mask = 0 }),
    };
}

pub fn initNull() Self {
    return Self{
        .pieces = @splat(.{ .mask = 0 }),
        .colours = @splat(.{ .mask = 0 }),
        .side_to_move = Color.White,
        .castle_rights = @splat(.{ .mask = 0 }),
        .ep_square = null,
        .halfmove_clock = 0,
        .fullmove_number = 0,
        .hist = @splat(0),
        .hash = 0,
        .attack_bitboards = @splat(.{ .mask = 0 }),
    };
}

pub fn fromFEN(
    position: []const u8,
    side_to_move: []const u8,
    castle_rights: []const u8,
    ep_square: []const u8,
    halfmove_clock: []const u8,
    fullmove_number: []const u8,
) ChessError!Self {
    var board: Self = .initNull();

    var sq: Square = 63;
    for (position) |char| {
        if (char == '/') {
            continue;
        } else if (char >= '1' and char <= '8') {
            sq -= char - '0';
            continue;
        }

        const color_index = @intFromBool(ascii.isLower(char));
        const piece_type = switch (ascii.toLower(char)) {
            'p' => PieceType.Pawn,
            'n' => PieceType.Knight,
            'b' => PieceType.Bishop,
            'r' => PieceType.Rook,
            'q' => PieceType.Queen,
            'k' => PieceType.King,
            else => null,
        };

        if (piece_type) |pt| {
            board.pieces[@intFromEnum(pt)].set(sq);
            board.colours[color_index].set(sq);

            board.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(pt) + sq];
            board.hash ^= zb_hash_const[384 + @as(usize, 64) * color_index + sq];
        }

        if (sq > 0) sq -= 1;
    }

    board.side_to_move = switch (side_to_move[0]) {
        'w' => Color.White,
        'b' => Color.Black,
        else => return ChessError.InvalidFEN,
    };

    if (board.side_to_move == Color.Black) {
        board.hash ^= zb_hash_const[512];
    }

    for (castle_rights) |char| {
        const color_index: usize = @intFromBool(ascii.isLower(char));
        var index: usize = 0;
        switch (ascii.toLower(char)) {
            'k' => index = 0,
            'q' => index = 7,
            'a'...'h' => index = char - 'a',
            else => {},
        }
        board.castle_rights[color_index].set(index);
        board.hash ^= zb_hash_const[@as(usize, 513) + index + color_index * 8];
    }

    board.ep_square = sqToIndex(ep_square);
    board.hash ^= if (board.ep_square) |s| zb_hash_const[@as(usize, 529) + s] else 0;

    board.halfmove_clock = fmt.parseInt(u8, halfmove_clock, 10) catch return ChessError.InvalidFEN;
    board.fullmove_number = fmt.parseInt(u16, fullmove_number, 10) catch return ChessError.InvalidFEN;

    return board;
}

test fromFEN {
    const fen_position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";
    const side_to_move = "w";
    const castle_rights = "KQkq";
    const ep_square = "-";
    const halfmove_clock = "0";
    const fullmove_number = "0";

    const board = try fromFEN(
        fen_position,
        side_to_move,
        castle_rights,
        ep_square,
        halfmove_clock,
        fullmove_number,
    );

    const startpos = Self.startPosition();

    try std.testing.expect(board.pieces[0].mask == startpos.pieces[0].mask);
    try std.testing.expect(board.pieces[1].mask == startpos.pieces[1].mask);
    try std.testing.expect(board.pieces[2].mask == startpos.pieces[2].mask);
    try std.testing.expect(board.pieces[3].mask == startpos.pieces[3].mask);
    try std.testing.expect(board.pieces[4].mask == startpos.pieces[4].mask);
    try std.testing.expect(board.pieces[5].mask == startpos.pieces[5].mask);
    try std.testing.expect(board.colours[0].mask == startpos.colours[0].mask);
    try std.testing.expect(board.colours[1].mask == startpos.colours[1].mask);
    try std.testing.expect(board.side_to_move == startpos.side_to_move);
    try std.testing.expect(board.castle_rights[0] == startpos.castle_rights[0]);
    try std.testing.expect(board.castle_rights[1] == startpos.castle_rights[1]);
    try std.testing.expect(board.ep_square == startpos.ep_square);
    try std.testing.expect(board.halfmove_clock == startpos.halfmove_clock);
    try std.testing.expect(board.fullmove_number == startpos.fullmove_number);
    try std.testing.expect(board.hash == startpos.hash);
}

pub fn isSquareOccupied(self: Self, sq: Square) bool {
    return self.colours[0].isSet(sq) or self.colours[1].isSet(sq);
}

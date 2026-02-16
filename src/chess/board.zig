const std = @import("std");
const builtin = @import("builtin");
const bit_set = std.bit_set;
const ascii = std.ascii;
const fmt = std.fmt;
const log = std.log;
const mem = std.mem;

const basic_types = @import("basic_types.zig");
const Bitboard = basic_types.Bitboard;
const Square = basic_types.Square;
const Color = basic_types.Color;
const PieceType = basic_types.PieceType;
const Move = basic_types.Move;
const Piece = basic_types.Piece;
const Direction = basic_types.Direction;

const errors = @import("errors.zig");
const ChessError = errors.ChessError;

const utils = @import("utils.zig");
const sqToIndex = utils.sqToIndex;
const indexToSq = utils.indexToSq;
const parseMove = utils.parseMove;

const rotl = std.math.rotl;
const cast = std.math.cast;

const Self = @This();

const zb_hash_const = blk: {
    @setEvalBranchQuota(1 << 16);
    var arr: [639]u64 = undefined;
    var rand = std.Random.DefaultPrng.init(0x9E3779B97F4A7C15);
    for (0..639) |i| {
        arr[i] = rand.next();
    }
    break :blk arr;
};

const PAWN_ATTACKS: [2][64]u64 = blk: {
    @setEvalBranchQuota(1 << 16);
    var arr: [2][64]u64 = .{ @splat(0), @splat(0) };

    var bb: u64 = 1 << 7;
    for (8..56) |i| {
        bb <<= 1;

        arr[0][i] |= (bb & 0xFEFEFEFEFEFEFEFE) << 7; // up 1, left 1
        arr[0][i] |= (bb & 0x7F7F7F7F7F7F7F7F) << 9; // up 1, right 1
    }

    bb = 1 << 7;
    for (8..56) |i| {
        bb <<= 1;

        arr[1][i] |= (bb & 0x7F7F7F7F7F7F7F7F) >> 7; // down 1, left 1
        arr[1][i] |= (bb & 0xFEFEFEFEFEFEFEFE) >> 9; // down 1, right 1
    }

    break :blk arr;
};

const KNIGHT_ATTACKS: [64]u64 = blk: {
    var arr: [64]u64 = @splat(0);
    var bb: u64 = 1;

    for (0..64) |i| {
        arr[i] |= (bb << 6) & 0x3F3F3F3F3F3F3F3F; // up 1, left 2
        arr[i] |= (bb << 10) & 0xFCFCFCFCFCFCFCFC; // up 1, right 2
        arr[i] |= (bb << 15) & 0x7F7F7F7F7F7F7F7F; // up 2, left 1
        arr[i] |= (bb << 17) & 0xFEFEFEFEFEFEFEFE; // up 2, right 1

        arr[i] |= (bb >> 6) & 0xFCFCFCFCFCFCFCFC; // down 1, right 2
        arr[i] |= (bb >> 10) & 0x3F3F3F3F3F3F3F3F; // down 1, left 2
        arr[i] |= (bb >> 15) & 0xFEFEFEFEFEFEFEFE; // down 2, right 1
        arr[i] |= (bb >> 17) & 0x7F7F7F7F7F7F7F7F; // down 2, left 1

        bb <<= 1;
    }

    break :blk arr;
};

const KING_ATTACKS: [64]u64 = blk: {
    @setEvalBranchQuota(1 << 16);
    var arr: [64]u64 = @splat(0);
    var bb: u64 = 1;

    for (0..64) |i| {
        arr[i] |= bb << 8; // up
        arr[i] |= bb >> 8; // down
        arr[i] |= (bb << 1) & 0x7F7F7F7F7F7F7F7F; // left
        arr[i] |= (bb >> 1) & 0xFEFEFEFEFEFEFEFE; // right
        arr[i] |= (bb << 7) & 0x7F7F7F7F7F7F7F7F; // up, left
        arr[i] |= (bb >> 7) & 0xFEFEFEFEFEFEFEFE; // down, right
        arr[i] |= (bb << 9) & 0x7F7F7F7F7F7F7F7F; // up, right
        arr[i] |= (bb >> 9) & 0xFEFEFEFEFEFEFEFE; // down, left

        bb <<= 1;
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

pub fn prettyPrint(self: Self) void {
    std.debug.print("\n\n  +-----------------+", .{});
    inline for (1..9) |i| {
        std.debug.print("\n{d} |", .{9 - i});
        inline for (0..8) |j| {
            const sq: Square = (8 - i) * 8 + j;
            if (self.getPieceAt(sq)) |piece| {
                const char: u8 = switch (piece.piece_type) {
                    .Pawn => 'p',
                    .Knight => 'n',
                    .Bishop => 'b',
                    .Rook => 'r',
                    .Queen => 'q',
                    .King => 'k',
                };
                std.debug.print(" {c}", .{if (piece.color == .White) ascii.toUpper(char) else char});
            } else std.debug.print(" .", .{});
        }
        std.debug.print(" |", .{});

        if (i == 1) {
            std.debug.print("  Side to move: {s}", .{if (self.side_to_move == .White) "White" else "Black"});
        } else if (i == 2) {
            var castle_str: [16]u8 = undefined;
            var idx: usize = 0;

            var castle_white = self.castle_rights[0].iterator(.{});
            var castle_black = self.castle_rights[1].iterator(.{});

            while (castle_white.next()) |pos| {
                castle_str[idx] = 'A' + @as(u8, @intCast(pos));
                idx += 1;
            }

            while (castle_black.next()) |pos| {
                castle_str[idx] = 'a' + @as(u8, @intCast(pos));
                idx += 1;
            }

            std.debug.print("  Castling rights: {s}", .{castle_str[0..idx]});
        } else if (i == 3) {
            const ep_sq_str = indexToSq(self.ep_square);
            std.debug.print("  En passant square: {s}", .{ep_sq_str});
        } else if (i == 4) {
            std.debug.print("  Halfmove clock: {d}", .{self.halfmove_clock});
        } else if (i == 5) {
            std.debug.print("  Fullmove number: {d}", .{self.fullmove_number});
        } else if (i == 6) {
            std.debug.print("  Internal Zobrist Hash: {X}", .{self.hash});
        }
    }
    std.debug.print("\n  +-----------------+\n    a b c d e f g h\n\n", .{});
}

pub fn startPosition() Self {
    var board: Self = .{
        .pieces = .{
            .{ .mask = 0x00FF00000000FF00 },
            .{ .mask = 0x4200000000000042 },
            .{ .mask = 0x2400000000000024 },
            .{ .mask = 0x8100000000000081 },
            .{ .mask = 0x0800000000000008 },
            .{ .mask = 0x1000000000000010 },
        },
        .colours = .{
            .{ .mask = 0x000000000000FFFF },
            .{ .mask = 0xFFFF000000000000 },
        },
        .side_to_move = .White,
        .castle_rights = @splat(.{ .mask = 0x81 }),
        .ep_square = null,
        .halfmove_clock = 0,
        .fullmove_number = 1,
        .hist = @splat(0),
        .hash = 0xA0A0C1A56FA72E08,
        .attack_bitboards = @splat(.{ .mask = 0 }),
    };

    _ = board.getAttackBitboards();
    return board;
}

pub fn initNull() Self {
    return Self{
        .pieces = @splat(.{ .mask = 0 }),
        .colours = @splat(.{ .mask = 0 }),
        .side_to_move = .White,
        .castle_rights = @splat(.{ .mask = 0 }),
        .ep_square = null,
        .halfmove_clock = 0,
        .fullmove_number = 0,
        .hist = @splat(0),
        .hash = 0,
        .attack_bitboards = @splat(.{ .mask = 0 }),
    };
}

pub fn copy(self: Self) Self {
    return Self{
        .pieces = self.pieces,
        .colours = self.colours,
        .side_to_move = self.side_to_move,
        .castle_rights = self.castle_rights,
        .ep_square = self.ep_square,
        .halfmove_clock = self.halfmove_clock,
        .fullmove_number = self.fullmove_number,
        .hist = self.hist,
        .hash = self.hash,
        .attack_bitboards = self.attack_bitboards,
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
            const num_empty: u8 = char - '0';

            if (sq < num_empty) {
                if (sq == 0 and num_empty == 1) {
                    break;
                } else {
                    if (!builtin.is_test) log.err("FEN position has too many empty squares", .{});
                    return ChessError.InvalidFEN;
                }
            }

            sq -= num_empty;
            continue;
        }

        const color_index = @intFromBool(ascii.isLower(char));
        const piece_type: PieceType = switch (ascii.toLower(char)) {
            'p' => .Pawn,
            'n' => .Knight,
            'b' => .Bishop,
            'r' => .Rook,
            'q' => .Queen,
            'k' => .King,
            else => {
                if (!builtin.is_test) log.err("invalid FEN character at square {s}: '{c}'", .{ indexToSq(sq), char });
                return ChessError.InvalidFEN;
            },
        };

        sq ^= 7; // flip horizontally to match internal representation

        board.pieces[@intFromEnum(piece_type)].set(sq);
        board.colours[color_index].set(sq);

        board.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(piece_type) + sq];
        board.hash ^= zb_hash_const[384 + @as(usize, 64) * color_index + sq];

        sq ^= 7; // flip back

        if (sq == 0) break;
        sq -= 1;
    }

    if (sq != 0) {
        if (!builtin.is_test) log.err("FEN position does not fill the board completely", .{});
        return ChessError.InvalidFEN;
    }

    board.side_to_move = switch (side_to_move[0]) {
        'w' => .White,
        'b' => .Black,
        else => {
            if (!builtin.is_test) log.err("invalid side to move in FEN: '{s}'", .{side_to_move});
            return ChessError.InvalidFEN;
        },
    };

    if (board.side_to_move == .Black) {
        board.hash ^= zb_hash_const[512];
    }

    for (castle_rights) |char| {
        const color_index: usize = @intFromBool(ascii.isLower(char));
        var index: usize = 0;
        switch (ascii.toLower(char)) {
            'k' => index = 0,
            'q' => index = 7,
            'a'...'h' => index = char - 'a',
            '-' => break,
            else => {
                if (!builtin.is_test) log.err("invalid castling right in FEN: '{c}'", .{char});
                return ChessError.InvalidFEN;
            },
        }
        board.castle_rights[color_index].set(index);
        board.hash ^= zb_hash_const[@as(usize, 513) + index + color_index * 8];
    }

    board.ep_square = sqToIndex(ep_square);
    if (board.ep_square) |s| {
        const rank = s / 8;
        if ((board.side_to_move == .White and rank != 5) or
            (board.side_to_move == .Black and rank != 2))
        {
            if (!builtin.is_test) log.err("invalid en passant square in FEN: '{s}'", .{ep_square});
            return ChessError.InvalidFEN;
        }
    } else if (!std.mem.eql(u8, ep_square, "-")) {
        if (!builtin.is_test) log.err("invalid en passant square in FEN: '{s}'", .{ep_square});
        return ChessError.InvalidFEN;
    }
    board.hash ^= if (board.ep_square) |s| zb_hash_const[@as(usize, 529) + s] else 0;

    board.halfmove_clock = fmt.parseInt(u8, halfmove_clock, 10) catch {
        if (!builtin.is_test) log.err("invalid halfmove clock in FEN: '{s}'", .{halfmove_clock});
        return ChessError.InvalidFEN;
    };

    board.fullmove_number = fmt.parseInt(u16, fullmove_number, 10) catch {
        if (!builtin.is_test) log.err("invalid fullmove number in FEN: '{s}'", .{fullmove_number});
        return ChessError.InvalidFEN;
    };

    _ = board.getAttackBitboards();
    return board;
}

test fromFEN {
    const position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR";
    const side_to_move = "w";
    const castle_rights = "KQkq";
    const ep_square = "-";
    const halfmove_clock = "0";
    const fullmove_number = "1";

    const board = try fromFEN(
        position,
        side_to_move,
        castle_rights,
        ep_square,
        halfmove_clock,
        fullmove_number,
    );

    const startpos = Self.startPosition();

    try std.testing.expectEqual(startpos.pieces[0].mask, board.pieces[0].mask);
    try std.testing.expectEqual(startpos.pieces[1].mask, board.pieces[1].mask);
    try std.testing.expectEqual(startpos.pieces[2].mask, board.pieces[2].mask);
    try std.testing.expectEqual(startpos.pieces[3].mask, board.pieces[3].mask);
    try std.testing.expectEqual(startpos.pieces[4].mask, board.pieces[4].mask);
    try std.testing.expectEqual(startpos.pieces[5].mask, board.pieces[5].mask);
    try std.testing.expectEqual(startpos.colours[0].mask, board.colours[0].mask);
    try std.testing.expectEqual(startpos.colours[1].mask, board.colours[1].mask);
    try std.testing.expectEqual(startpos.side_to_move, board.side_to_move);
    try std.testing.expectEqual(startpos.castle_rights[0], board.castle_rights[0]);
    try std.testing.expectEqual(startpos.castle_rights[1], board.castle_rights[1]);
    try std.testing.expectEqual(startpos.ep_square, board.ep_square);
    try std.testing.expectEqual(startpos.halfmove_clock, board.halfmove_clock);
    try std.testing.expectEqual(startpos.fullmove_number, board.fullmove_number);
    try std.testing.expectEqual(startpos.hash, board.hash);

    const bad_position = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPP/RNB";
    const bad_stm = "r";
    const bad_castle = "KQkp";
    const bad_ep = "i9";
    const bad_halfmove = "qwerty";
    const bad_fullmove = "asdfgh";

    try std.testing.expectError(ChessError.InvalidFEN, fromFEN(bad_position, side_to_move, castle_rights, ep_square, halfmove_clock, fullmove_number));
    try std.testing.expectError(ChessError.InvalidFEN, fromFEN(position, bad_stm, castle_rights, ep_square, halfmove_clock, fullmove_number));
    try std.testing.expectError(ChessError.InvalidFEN, fromFEN(position, side_to_move, bad_castle, ep_square, halfmove_clock, fullmove_number));
    try std.testing.expectError(ChessError.InvalidFEN, fromFEN(position, side_to_move, castle_rights, bad_ep, halfmove_clock, fullmove_number));
    try std.testing.expectError(ChessError.InvalidFEN, fromFEN(position, side_to_move, castle_rights, ep_square, bad_halfmove, fullmove_number));
    try std.testing.expectError(ChessError.InvalidFEN, fromFEN(position, side_to_move, castle_rights, ep_square, halfmove_clock, bad_fullmove));
}

pub fn isSquareOccupied(self: Self, sq: Square) bool {
    return self.colours[0].isSet(sq) or self.colours[1].isSet(sq);
}

pub fn getPieceAt(self: Self, sq: Square) ?Piece {
    if (!self.isSquareOccupied(sq)) return null;

    return .{
        .color = if (self.colours[0].isSet(sq)) .White else .Black,
        .piece_type = blk: {
            if (self.pieces[0].isSet(sq)) break :blk .Pawn;
            if (self.pieces[1].isSet(sq)) break :blk .Knight;
            if (self.pieces[2].isSet(sq)) break :blk .Bishop;
            if (self.pieces[3].isSet(sq)) break :blk .Rook;
            if (self.pieces[4].isSet(sq)) break :blk .Queen;
            break :blk .King;
        },
    };
}

pub fn makeMove(self: *Self, move: Move) ChessError!void {
    if (!self.isSquareOccupied(move.from)) {
        if (!builtin.is_test) log.err("invalid move: no piece at square {s} to move", .{indexToSq(move.from)});
        return ChessError.InvalidMove;
    }

    if (move.from == move.to) {
        self.side_to_move = switch (self.side_to_move) {
            .White => .Black,
            .Black => .White,
        };
        self.hash ^= zb_hash_const[512];
        return; // null move
    }

    const piece_moved = self.getPieceAt(move.from).?;

    if (piece_moved.color != self.side_to_move) {
        if (!builtin.is_test) log.err("invalid move: piece at square {s} does not belong to side to move", .{indexToSq(move.from)});
        return ChessError.InvalidMove;
    }

    self.pieces[@intFromEnum(piece_moved.piece_type)].unset(move.from);
    self.colours[@intFromEnum(piece_moved.color)].unset(move.from);

    self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(piece_moved.piece_type) + move.from];
    self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_moved.color) + move.from];

    const piece_to_place = move.promotion orelse piece_moved.piece_type;

    self.halfmove_clock += 1;
    self.fullmove_number += @intFromBool(self.side_to_move == .Black);

    if (self.isSquareOccupied(move.to)) {
        const piece_captured = self.getPieceAt(move.to).?;

        self.pieces[@intFromEnum(piece_captured.piece_type)].unset(move.to);
        self.colours[@intFromEnum(piece_captured.color)].unset(move.to);

        self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(piece_captured.piece_type) + move.to];
        self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_captured.color) + move.to];

        self.halfmove_clock = 0;

        if (piece_captured.piece_type == .King) {
            if (!builtin.is_test) log.err("invalid move: cannot capture the king at square {s}", .{indexToSq(move.to)});
            return ChessError.InvalidMove;
        } else if (piece_captured.piece_type == .Rook and piece_moved.piece_type != .King) {
            if (move.to & 56 == 56 * @as(u8, @intFromEnum(piece_captured.color))) {
                const index = move.to & 7 ^ 7;
                if (self.castle_rights[@intFromEnum(piece_captured.color)].isSet(index)) {
                    self.castle_rights[@intFromEnum(piece_captured.color)].unset(index);
                    self.hash ^= zb_hash_const[@as(usize, 513) + index + @as(u8, @intFromEnum(piece_captured.color)) * 8];
                }
            }
        }
    }

    self.pieces[@intFromEnum(piece_to_place)].set(move.to);
    self.colours[@intFromEnum(piece_moved.color)].set(move.to);

    self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(piece_to_place) + move.to];
    self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_moved.color) + move.to];

    if (piece_moved.piece_type == .Pawn) {
        self.halfmove_clock = 0;

        const ep_sq = if (self.side_to_move == .White) move.to - 8 else move.to + 8;
        if ((move.to -% move.from) & 31 == 16) {
            // double pawn push
            const opp_pawns = self.pieces[@intFromEnum(PieceType.Pawn)].intersectWith(self.colours[@intFromEnum(self.side_to_move) ^ 1]);
            if (opp_pawns.isSet(move.to - 1) or opp_pawns.isSet(move.to + 1)) {
                self.ep_square = ep_sq;
                self.hash ^= zb_hash_const[@as(usize, 529) + ep_sq];
            }
        } else if (move.to == self.ep_square) {
            // en passant capture

            self.pieces[@intFromEnum(PieceType.Pawn)].unset(ep_sq);
            self.colours[@intFromEnum(self.side_to_move) ^ 1].unset(ep_sq);

            self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(PieceType.Pawn) + ep_sq];
            self.hash ^= zb_hash_const[384 + @as(usize, 64) * (@intFromEnum(self.side_to_move) ^ 1) + ep_sq];
            self.hash ^= zb_hash_const[@as(usize, 529) + self.ep_square.?];
            self.ep_square = null;
        }
    } else {
        if (self.ep_square) |s| {
            self.hash ^= zb_hash_const[@as(usize, 529) + s];
        }
        self.ep_square = null;

        if (piece_moved.piece_type == .Rook) {
            if (move.from & 56 == 56 * @as(u8, @intFromEnum(piece_moved.color))) {
                const index = move.from & 7 ^ 7;
                if (self.castle_rights[@intFromEnum(piece_moved.color)].isSet(index)) {
                    self.castle_rights[@intFromEnum(piece_moved.color)].unset(index);
                    self.hash ^= zb_hash_const[@as(usize, 513) + index + @as(u8, @intFromEnum(piece_moved.color)) * 8];
                }
            }
        } else if (piece_moved.piece_type == .King) {
            if (self.getPieceAt(move.to) != null and self.getPieceAt(move.to).?.color != self.side_to_move) {
                if (!builtin.is_test) log.err("invalid move: cannot castle through or into check at square {s}", .{indexToSq(move.to)});
                return ChessError.InvalidMove;
            }
            const color_index: u8 = @intFromEnum(piece_moved.color);

            var it = self.castle_rights[color_index].iterator(.{});
            while (it.next()) |pos| {
                self.castle_rights[color_index].unset(pos);
                self.hash ^= zb_hash_const[@as(usize, 513) + pos + color_index * 8];
            }

            if (move.to > move.from) {
                self.pieces[@intFromEnum(PieceType.Rook)].set(move.to - 1);
                self.colours[@intFromEnum(piece_moved.color)].set(move.to - 1);

                self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(PieceType.Rook) + move.to - 1];
                self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_moved.color) + move.to - 1];

                if (self.pieces[@intFromEnum(PieceType.Rook)].isSet(move.to + 1)) {
                    self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(PieceType.Rook) + move.to + 1];
                    self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_moved.color) + move.to + 1];
                }

                self.pieces[@intFromEnum(PieceType.Rook)].unset(move.to + 1);
                self.colours[@intFromEnum(piece_moved.color)].unset(move.to + 1);
            } else {
                self.pieces[@intFromEnum(PieceType.Rook)].set(move.to + 1);
                self.colours[@intFromEnum(piece_moved.color)].set(move.to + 1);

                self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(PieceType.Rook) + move.to + 1];
                self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_moved.color) + move.to + 1];

                if (self.pieces[@intFromEnum(PieceType.Rook)].isSet(move.to - 1)) {
                    self.hash ^= zb_hash_const[@as(usize, 64) * @intFromEnum(PieceType.Rook) + move.to - 1];
                    self.hash ^= zb_hash_const[384 + @as(usize, 64) * @intFromEnum(piece_moved.color) + move.to - 1];
                }

                self.pieces[@intFromEnum(PieceType.Rook)].unset(move.to - 1);
                self.colours[@intFromEnum(piece_moved.color)].unset(move.to - 1);
            }
        }
    }

    self.side_to_move = switch (self.side_to_move) {
        .White => .Black,
        .Black => .White,
    };
    self.hash ^= zb_hash_const[512];

    _ = getAttackBitboards(self); // TODO: make attack bitboard updates incremental (only update attacks for sliders and pieces that moved and/or were captured)
    if (self.isSquareAttacked(cast(u8, self.pieces[5].intersectWith(self.colours[@intFromEnum(self.side_to_move)]).findFirstSet().?).?, switch (self.side_to_move) {
        .White => .Black,
        .Black => .White,
    })) {
        if (!builtin.is_test) log.err("invalid move: cannot move into check", .{});
        return ChessError.InvalidMove;
    }

    inline for (0..6) |i| self.hist[i] = if (i == 0) self.hash else self.hist[i - 1];
}

test makeMove {
    var board = Self.startPosition();

    try board.makeMove(parseMove("e2e4"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR", "b", "KQkq", "-", "0", "1")).hash, board.hash);
    try std.testing.expectEqual(1, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("e7e5"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR", "w", "KQkq", "-", "0", "2")).hash, board.hash);
    try std.testing.expectEqual(2, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try std.testing.expectError(ChessError.InvalidMove, board.makeMove(parseMove("e5e4")));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR", "w", "KQkq", "-", "0", "2")).hash, board.hash);

    try board.makeMove(parseMove("d2d4"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/4p3/3PP3/8/PPP2PPP/RNBQKBNR", "b", "KQkq", "-", "0", "2")).hash, board.hash);
    try std.testing.expectEqual(2, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("e5d4"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/3pP3/8/PPP2PPP/RNBQKBNR", "w", "KQkq", "-", "0", "3")).hash, board.hash);
    try std.testing.expectEqual(3, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("c2c3"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/3pP3/2P5/PP3PPP/RNBQKBNR", "b", "KQkq", "-", "0", "3")).hash, board.hash);
    try std.testing.expectEqual(3, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try std.testing.expectError(ChessError.InvalidMove, board.makeMove(parseMove("d1h4")));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/3pP3/2P5/PP3PPP/RNBQKBNR", "b", "KQkq", "-", "0", "3")).hash, board.hash);
    try std.testing.expectEqual(3, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try std.testing.expectError(ChessError.InvalidMove, board.makeMove(parseMove("d6h3")));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/3pP3/2P5/PP3PPP/RNBQKBNR", "b", "KQkq", "-", "0", "3")).hash, board.hash);
    try std.testing.expectEqual(3, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("d4c3"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/4P3/2p5/PP3PPP/RNBQKBNR", "w", "KQkq", "-", "0", "4")).hash, board.hash);
    try std.testing.expectEqual(4, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("f1c4"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/2B1P3/2p5/PP3PPP/RNBQK1NR", "b", "KQkq", "-", "1", "4")).hash, board.hash);
    try std.testing.expectEqual(4, board.fullmove_number);
    try std.testing.expectEqual(1, board.halfmove_clock);

    try board.makeMove(parseMove("c3b2"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/2B1P3/8/Pp3PPP/RNBQK1NR", "w", "KQkq", "-", "0", "5")).hash, board.hash);
    try std.testing.expectEqual(5, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("g1f3"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/2B1P3/5N2/Pp3PPP/RNBQK2R", "b", "KQkq", "-", "1", "5")).hash, board.hash);
    try std.testing.expectEqual(5, board.fullmove_number);
    try std.testing.expectEqual(1, board.halfmove_clock);

    try board.makeMove(parseMove("b2a1q"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/2B1P3/5N2/P4PPP/qNBQK2R", "w", "Kkq", "-", "0", "6")).hash, board.hash);
    try std.testing.expectEqual(6, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("e1g1"));
    try std.testing.expectEqual((try fromFEN("rnbqkbnr/pppp1ppp/8/8/2B1P3/5N2/P4PPP/qNBQ1RK1", "b", "kq", "-", "1", "6")).hash, board.hash);
    try std.testing.expectEqual(6, board.fullmove_number);
    try std.testing.expectEqual(1, board.halfmove_clock);

    try board.makeMove(parseMove("g8f6"));
    try std.testing.expectEqual((try fromFEN("rnbqkb1r/pppp1ppp/5n2/8/2B1P3/5N2/P4PPP/qNBQ1RK1", "w", "kq", "-", "2", "7")).hash, board.hash);
    try std.testing.expectEqual(7, board.fullmove_number);
    try std.testing.expectEqual(2, board.halfmove_clock);

    try board.makeMove(parseMove("e4e5"));
    try std.testing.expectEqual((try fromFEN("rnbqkb1r/pppp1ppp/5n2/4P3/2B5/5N2/P4PPP/qNBQ1RK1", "b", "kq", "-", "0", "7")).hash, board.hash);
    try std.testing.expectEqual(7, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("d7d5"));
    try std.testing.expectEqual((try fromFEN("rnbqkb1r/ppp2ppp/5n2/3pP3/2B5/5N2/P4PPP/qNBQ1RK1", "w", "kq", "d6", "0", "8")).hash, board.hash);
    try std.testing.expectEqual(8, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);

    try board.makeMove(parseMove("e5d6"));
    try std.testing.expectEqual((try fromFEN("rnbqkb1r/ppp2ppp/3P1n2/8/2B5/5N2/P4PPP/qNBQ1RK1", "b", "kq", "-", "0", "8")).hash, board.hash);
    try std.testing.expectEqual(8, board.fullmove_number);
    try std.testing.expectEqual(0, board.halfmove_clock);
}

fn getRayAttacks(sq: u8, blockers: Bitboard, dir: Direction) Bitboard {
    var gen: Bitboard = .initEmpty();
    var pro = blockers.complement();

    gen.set(sq);

    const avoid: u64 = switch (dir) {
        .North => 0xFFFFFFFFFFFFFF00,
        .South => 0x00FFFFFFFFFFFFFF,
        .East => 0xFEFEFEFEFEFEFEFE,
        .West => 0x7F7F7F7F7F7F7F7F,
        .NorthEast => 0x7F7F7F7F7F7F7F00,
        .NorthWest => 0xFEFEFEFEFEFEFE00,
        .SouthEast => 0x007F7F7F7F7F7F7F,
        .SouthWest => 0x00FEFEFEFEFEFEFE,
    };

    pro.mask &= avoid;

    gen.setUnion(pro.intersectWith(.{ .mask = rotl(u64, gen.mask, @intFromEnum(dir)) }));
    pro.setIntersection(.{ .mask = rotl(u64, pro.mask, @intFromEnum(dir)) });

    gen.setUnion(pro.intersectWith(.{ .mask = rotl(u64, gen.mask, @intFromEnum(dir) * 2) }));
    pro.setIntersection(.{ .mask = rotl(u64, pro.mask, @intFromEnum(dir) * 2) });

    return .{ .mask = rotl(u64, gen.unionWith(pro.intersectWith(.{ .mask = rotl(u64, gen.mask, @intFromEnum(dir) * 4) })).mask, @intFromEnum(dir)) & avoid };
}

fn getAttackBitboards(self: *Self) [64]Bitboard {
    const blockers = self.colours[0].unionWith(self.colours[1]);
    var attack_bitboards: [64]Bitboard = @splat(.{ .mask = 0 });
    var it = blockers.iterator(.{});

    while (it.next()) |s| {
        const sq = @as(u8, @intCast(s));
        const piece = self.getPieceAt(sq).?;

        if (piece.piece_type == .Bishop or piece.piece_type == .Queen) {
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .NorthEast));
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .NorthWest));
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .SouthEast));
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .SouthWest));
        }

        if (piece.piece_type == .Rook or piece.piece_type == .Queen) {
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .North));
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .South));
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .East));
            attack_bitboards[sq].setUnion(getRayAttacks(sq, blockers, .West));
        }

        if (attack_bitboards[sq].mask == 0) {
            switch (piece.piece_type) {
                .Pawn => attack_bitboards[sq].mask = PAWN_ATTACKS[@intFromEnum(piece.color)][sq],
                .Knight => attack_bitboards[sq].mask = KNIGHT_ATTACKS[sq],
                .King => attack_bitboards[sq].mask = KING_ATTACKS[sq],
                else => unreachable,
            }
        }
    }

    self.attack_bitboards = attack_bitboards;
    return attack_bitboards;
}

test getAttackBitboards {
    const board = Self.startPosition();

    try std.testing.expect(board.attack_bitboards[0].mask == 0x0000000000000102);
    try std.testing.expect(board.attack_bitboards[1].mask == 0x0000000000050800);
    try std.testing.expect(board.attack_bitboards[2].mask == 0x0000000000000A00);
    try std.testing.expect(board.attack_bitboards[3].mask == 0x0000000000001C14);
    try std.testing.expect(board.attack_bitboards[4].mask == 0x0000000000003828);
    try std.testing.expect(board.attack_bitboards[5].mask == 0x0000000000005000);
    try std.testing.expect(board.attack_bitboards[6].mask == 0x0000000000A01000);
    try std.testing.expect(board.attack_bitboards[7].mask == 0x0000000000008040);
    try std.testing.expect(board.attack_bitboards[8].mask == 0x0000000000020000);
    try std.testing.expect(board.attack_bitboards[9].mask == 0x0000000000050000);
    try std.testing.expect(board.attack_bitboards[10].mask == 0x00000000000A0000);
    try std.testing.expect(board.attack_bitboards[11].mask == 0x0000000000140000);
    try std.testing.expect(board.attack_bitboards[12].mask == 0x0000000000280000);
    try std.testing.expect(board.attack_bitboards[13].mask == 0x0000000000500000);
    try std.testing.expect(board.attack_bitboards[14].mask == 0x0000000000A00000);
    try std.testing.expect(board.attack_bitboards[15].mask == 0x0000000000400000);

    inline for (16..48) |i| try std.testing.expect(board.attack_bitboards[i].mask == 0);

    try std.testing.expect(board.attack_bitboards[48].mask == 0x0000020000000000);
    try std.testing.expect(board.attack_bitboards[49].mask == 0x0000050000000000);
    try std.testing.expect(board.attack_bitboards[50].mask == 0x00000A0000000000);
    try std.testing.expect(board.attack_bitboards[51].mask == 0x0000140000000000);
    try std.testing.expect(board.attack_bitboards[52].mask == 0x0000280000000000);
    try std.testing.expect(board.attack_bitboards[53].mask == 0x0000500000000000);
    try std.testing.expect(board.attack_bitboards[54].mask == 0x0000A00000000000);
    try std.testing.expect(board.attack_bitboards[55].mask == 0x0000400000000000);
    try std.testing.expect(board.attack_bitboards[56].mask == 0x0201000000000000);
    try std.testing.expect(board.attack_bitboards[57].mask == 0x0008050000000000);
    try std.testing.expect(board.attack_bitboards[58].mask == 0x000A000000000000);
    try std.testing.expect(board.attack_bitboards[59].mask == 0x141C000000000000);
    try std.testing.expect(board.attack_bitboards[60].mask == 0x2838000000000000);
    try std.testing.expect(board.attack_bitboards[61].mask == 0x0050000000000000);
    try std.testing.expect(board.attack_bitboards[62].mask == 0x0010A00000000000);
    try std.testing.expect(board.attack_bitboards[63].mask == 0x4080000000000000);
}

pub fn isSquareAttacked(self: Self, sq: Square, by_color: Color) bool {
    for (0..64, self.attack_bitboards) |i, attacks| if (attacks.isSet(sq) and self.colours[@intFromEnum(by_color)].isSet(i)) return true;
    return false;
}

pub fn generatePseudolegalMoves(self: Self, allocator: mem.Allocator) !std.ArrayList(Move) {
    var moves = try std.ArrayList(Move).initCapacity(allocator, 256);

    for (self.attack_bitboards, 0..64) |attacks, f| {
        var it = attacks.iterator(.{});
        const from = cast(u8, f).?;

        const piece = self.getPieceAt(from) orelse continue;
        if (piece.color != self.side_to_move) continue;
        while (it.next()) |t| {
            const to = cast(u8, t).?;
            if (piece.piece_type == .Pawn and (to / 8 == 0 or to / 8 == 7)) {
                try moves.append(allocator, .{ .from = from, .to = to, .promotion = .Queen });
                try moves.append(allocator, .{ .from = from, .to = to, .promotion = .Rook });
                try moves.append(allocator, .{ .from = from, .to = to, .promotion = .Bishop });
                try moves.append(allocator, .{ .from = from, .to = to, .promotion = .Knight });

                continue;
            }

            try moves.append(allocator, .{ .from = from, .to = to, .promotion = null });
        }
    }

    return moves;
}

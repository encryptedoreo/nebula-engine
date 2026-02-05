const std = @import("std");
const builtin = @import("builtin");
const bit_set = std.bit_set;
const ascii = std.ascii;
const fmt = std.fmt;
const log = std.log;

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
const indexToSq = utils.indexToSq;
const parseMove = utils.parseMove;

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
    const PAWN_MASK = 0x00FF00000000FF00;
    const KNIGHT_MASK = 0x4200000000000042;
    const BISHOP_MASK = 0x2400000000000024;
    const ROOK_MASK = 0x8100000000000081;
    const QUEEN_MASK = 0x0800000000000008;
    const KING_MASK = 0x1000000000000010;

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
        .side_to_move = .White,
        .castle_rights = @splat(castle_rights),
        .ep_square = null,
        .halfmove_clock = 0,
        .fullmove_number = 1,
        .hist = @splat(0),
        .hash = 0xA0A0C1A56FA72E08,
    };
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
                if (!builtin.is_test) log.err("Invalid FEN character at square {s}: '{c}'", .{ indexToSq(sq), char });
                return ChessError.InvalidFEN;
            },
        };

        sq ^= 7; // flip square to match internal representation

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
            if (!builtin.is_test) log.err("Invalid side to move in FEN: '{s}'", .{side_to_move});
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
            else => {
                if (!builtin.is_test) log.err("Invalid castling right in FEN: '{c}'", .{char});
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
            if (!builtin.is_test) log.err("Invalid en passant square in FEN: '{s}'", .{ep_square});
            return ChessError.InvalidFEN;
        }
    } else if (!std.mem.eql(u8, ep_square, "-")) {
        if (!builtin.is_test) log.err("Invalid en passant square in FEN: '{s}'", .{ep_square});
        return ChessError.InvalidFEN;
    }
    board.hash ^= if (board.ep_square) |s| zb_hash_const[@as(usize, 529) + s] else 0;

    board.halfmove_clock = fmt.parseInt(u8, halfmove_clock, 10) catch {
        if (!builtin.is_test) log.err("Invalid halfmove clock in FEN: '{s}'", .{halfmove_clock});
        return ChessError.InvalidFEN;
    };

    board.fullmove_number = fmt.parseInt(u16, fullmove_number, 10) catch {
        if (!builtin.is_test) log.err("Invalid fullmove number in FEN: '{s}'", .{fullmove_number});
        return ChessError.InvalidFEN;
    };

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
        if (!builtin.is_test) log.err("Invalid move: no piece at square {s} to move", .{indexToSq(move.from)});
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
        if (!builtin.is_test) log.err("Invalid move: piece at square {s} does not belong to side to move", .{indexToSq(move.from)});
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
            if (!builtin.is_test) log.err("Invalid move: cannot capture the king at square {s}", .{indexToSq(move.to)});
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

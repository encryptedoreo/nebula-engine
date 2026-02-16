const std = @import("std");
const heap = std.heap;
const io = std.io;
const mem = std.mem;
const ascii = std.ascii;

pub const UCIError = @import("errors.zig").UCIError;
pub const Board = @import("chess").Board;

const cast = std.math.cast;

const Self = @This();

allocator: mem.Allocator,
stdin: *io.Reader,
stdout: *io.Writer,
position: Board = .initNull(),
is_searching: std.atomic.Value(bool) = .init(false),
thread: ?std.Thread = null,

pub fn processNextCommand(self: *Self) UCIError!void {
    while (self.stdin.takeDelimiterExclusive('\n')) |line| : (self.stdin.toss(1)) {
        var it = mem.tokenizeAny(u8, line, &ascii.whitespace);

        while (it.next()) |token| {
            if (mem.eql(u8, token, "uci")) {
                try self.stdout.writeAll("id name Nebula 1.0\n");
                try self.stdout.writeAll("id author EncryptedOreo\n");

                // to silence warnings from GUIs
                try self.stdout.writeAll("option name Hash type spin default 1 min 1 max 1\n");
                try self.stdout.writeAll("option name Threads type spin default 1 min 1 max 1\n");

                try self.stdout.writeAll("uciok\n");
            } else if (mem.eql(u8, token, "isready")) {
                try self.stdout.writeAll("readyok\n");
            } else if (mem.eql(u8, token, "position")) {
                if (mem.eql(u8, it.next().?, "startpos")) {
                    self.position = Board.startPosition();
                } else {
                    const position = it.next().?;
                    const side_to_move = it.next().?;
                    const castle_rights = it.next().?;
                    const ep_square = it.next().?;
                    const halfmove_clock = it.next().?;
                    const fullmove_number = it.next().?;

                    self.position = Board.fromFEN(
                        position,
                        side_to_move,
                        castle_rights,
                        ep_square,
                        halfmove_clock,
                        fullmove_number,
                    ) catch return UCIError.InvalidPosition;
                }
            } else if (mem.eql(u8, token, "go")) {
                if (self.is_searching.load(.acquire)) continue;
                self.is_searching.store(true, .release);

                if (mem.eql(u8, it.next().?, "perft")) {
                    const depth_str = it.next().?;
                    const depth = std.fmt.parseInt(usize, depth_str, 10) catch return;

                    self.thread = std.Thread.spawn(.{}, perft, .{ self, depth }) catch return;
                }
            } else if (mem.eql(u8, token, "stop")) {
                self.is_searching.store(false, .release);
                if (self.thread) |thread| thread.join();
                self.thread = null;
            } else if (mem.eql(u8, token, "quit")) {
                self.is_searching.store(false, .release);
                if (self.thread) |thread| thread.join();
                self.thread = null;
                return UCIError.ExitOK;
            }
        }

        try self.stdout.flush();
    } else |err| switch (err) {
        error.EndOfStream => return,
        error.StreamTooLong => return UCIError.BufferOverflow,
        else => return UCIError.ReadFailed,
    }
}

fn perft(self: *Self, depth: usize) !void {
    var arena: std.heap.ArenaAllocator = .init(self.allocator);
    defer arena.deinit();

    for (1..depth + 1) |d| {
        const start_time = std.time.milliTimestamp();
        const nodes = try self._perft(d, arena.allocator());
        const end_time = std.time.milliTimestamp();

        try self.stdout.print("info depth {} nodes {} nps {}\n", .{ d, nodes, @divFloor(cast(i64, nodes * 1000).?, @max(1, end_time - start_time)) });
        try self.stdout.flush();
    }

    self.is_searching.store(false, .release);
}

fn _perft(self: *Self, depth: usize, allocator: mem.Allocator) !usize {
    if (depth == 0) return 1;
    const legal_moves = try self.position.generatePseudolegalMoves(allocator);
    var total_nodes: usize = 0;

    for (legal_moves.items) |move| {
        const board = self.position.copy();
        try self.position.makeMove(move);
        if (!self.position.isLegal()) {
            self.position = board;
            continue;
        }

        total_nodes += try self._perft(depth - 1, allocator);
        self.position = board;
    }

    return total_nodes;
}

const std = @import("std");
const heap = std.heap;
const io = std.io;
const mem = std.mem;
const ascii = std.ascii;

pub const UCIError = @import("errors.zig").UCIError;
pub const Board = @import("chess").Board;

const Self = @This();

allocator: mem.Allocator,
stdin: *io.Reader,
stdout: *io.Writer,
position: Board = .initNull(),

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
            } else if (mem.eql(u8, token, "quit")) {
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

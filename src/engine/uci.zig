const std = @import("std");
const heap = std.heap;
const io = std.io;
const mem = std.mem;
const ascii = std.ascii;

const UCIError = @import("errors.zig").UCIError;
const Board = @import("chess").Board;

const Self = @This();

allocator: std.heap.Allocator,
stdin: io.Reader,
stdout: io.Writer,
position: Board,

pub fn processNextCommand(self: *Self) UCIError!void {
    while (self.stdin.takeDelimiterExclusive('\n')) |line| : (self.stdin.toss(1)) {
        var it = mem.tokenizeAny(u8, line, &ascii.whitespace);

        while (it.next()) |token| {
            if (mem.eql(u8, token, "uci")) {
                try self.stdout.print("id name Nebula 1.0");
                try self.stdout.print("id author EncryptedOreo");

                // to silence warnings from GUIs
                try self.stdout.print("option name Hash type spin default 1 min 1 max 1");
                try self.stdout.print("option name Threads type spin default 1 min 1 max 1");

                try self.stdout.print("uciok");
            } else if (mem.eql(u8, token, "isready")) {
                try self.stdout.print("readyok");
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
    } else |err| switch (err) {
        error.EndOfStream => return,
        error.StreamTooLong => return UCIError.BufferOverflow,
        else => return UCIError.ReadFailed,
    }
}

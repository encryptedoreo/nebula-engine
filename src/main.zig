const std = @import("std");
const builtin = @import("builtin");

const log = std.log;
const fs = std.fs;
const heap = std.heap;

const engine = @import("engine");
const UCI = engine.UCI;
const UCIError = engine.UCIError;

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

const BUF_SIZE = 65536;

pub fn main() !void {
    const allocator, const is_debug = gpa: {
        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ heap.smp_allocator, false },
        };
    };

    defer if (is_debug) {
        switch (debug_allocator.deinit()) {
            .leak => log.warn("Memory was leaked!", .{}),
            .ok => log.debug("There were no memory leaks.", .{}),
        }
    };

    var stdin_buf = allocator.alloc(u8, BUF_SIZE) catch {
        log.err("Failed to allocate stdin buffer", .{});
        return;
    };

    var stdout_buf = allocator.alloc(u8, BUF_SIZE) catch {
        log.err("Failed to allocate stdout buffer", .{});
        return;
    };

    var stdin_reader = fs.File.stdin().reader(&stdin_buf);
    var stdout_writer = fs.File.stdout().writer(&stdout_buf);

    const stdin = &stdin_reader.interface;
    const stdout = &stdout_writer.interface;

    var uci: UCI = .{
        .allocator = allocator,
        .stdin = stdin,
        .stdout = stdout,
    };

    while (true) uci.processNextCommand() catch |err| switch (err) {
        UCIError.ExitOK => return,
        else => return err,
    };
}

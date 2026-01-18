const std = @import("std");

pub fn main() !void {
    var stdout = std.fs.File.stdout();
    try stdout.writeAll("Hello, Nebula Engine!\n");
}

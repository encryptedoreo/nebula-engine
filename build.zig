const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const chess_mod = b.addModule("chess", .{
        .root_source_file = b.path("src/chess/root.zig"),
        .target = target,
    });

    const engine_mod = b.addModule("engine", .{
        .root_source_file = b.path("src/engine/root.zig"),
        .target = target,
    });

    const exe = b.addExecutable(.{
        .name = "nebula_engine",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "chess", .module = chess_mod },
                .{ .name = "engine", .module = engine_mod },
            },
        }),
    });

    b.installArtifact(exe);

    const run_step = b.step("run", "Run the program");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const exe_tests = b.addTest(.{ .root_module = exe.root_module });
    const chess_mod_tests = b.addTest(.{ .root_module = chess_mod });
    const engine_mod_tests = b.addTest(.{ .root_module = engine_mod });

    const run_exe_tests = b.addRunArtifact(exe_tests);
    const run_chess_mod_tests = b.addRunArtifact(chess_mod_tests);
    const run_engine_mod_tests = b.addRunArtifact(engine_mod_tests);

    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_exe_tests.step);
    test_step.dependOn(&run_chess_mod_tests.step);
    test_step.dependOn(&run_engine_mod_tests.step);
}

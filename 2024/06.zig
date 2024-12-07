const std = @import("std");

const Vec = struct {
    x: usize,
    y: usize,
};

const Direction = enum {
    Up,
    Down,
    Left,
    Right,

    fn turn(self: Direction) Direction {
        return switch (self) {
            .Up => .Right,
            .Right => .Down,
            .Down => .Left,
            .Left => .Up,
        };
    }

    fn step(self: Direction, player: *Vec) void {
        switch (self) {
            .Up => player.y -= 1,
            .Right => player.x += 1,
            .Down => player.y += 1,
            .Left => player.x -= 1,
        }
    }

    fn stepBack(self: Direction, player: *Vec) void {
        switch (self) {
            .Up => player.y += 1,
            .Right => player.x -= 1,
            .Down => player.y -= 1,
            .Left => player.x += 1,
        }
    }
};

pub fn main() !void {
    const in = std.io.getStdIn().reader();

    var msg_buf: [200]u8 = undefined;

    var blocks = std.ArrayList(Vec).init(std.heap.page_allocator);
    var player = Vec{ .x = 0, .y = 0 };
    var gridBounds = Vec{ .x = 0, .y = 0 };

    var y: usize = 0;
    while (try in.readUntilDelimiterOrEof(&msg_buf, '\n')) |line| : (y += 1) {
        for (line, 0..) |char, x| {
            gridBounds = Vec{ .x = x, .y = y };

            switch (char) {
                '#' => try blocks.append(.{ .x = x, .y = y }),
                '^' => player = .{ .x = x, .y = y },
                else => continue,
            }
        }
    }

    var part1 = try findExitPath(player, gridBounds, blocks);
    std.debug.print("{d}\n", .{part1.?.count()});

    var part2: usize = 0;
    var iter = part1.?.iterator();
    while (iter.next()) |data| {
        if (data.key_ptr.y == player.y and data.key_ptr.x == player.x) {
            continue;
        }

        try blocks.append(.{ .x = data.key_ptr.x, .y = data.key_ptr.y });

        if (try findExitPath(player, gridBounds, blocks) == null) {
            part2 += 1;
        }

        _ = blocks.pop();
    }

    std.debug.print("{d}", .{part2});
}

fn findExitPath(initialPlayer: Vec, gridBounds: Vec, blocks: std.ArrayList(Vec)) !?std.AutoHashMap(Vec, u32) {
    var seenPositions = std.AutoHashMap(Vec, u32).init(std.heap.page_allocator);
    var direction = Direction.Up;
    var player = initialPlayer;

    while (player.x <= gridBounds.x and player.y <= gridBounds.y and player.x > 0 and player.y > 0) {
        direction.step(&player);

        if (containsVec(blocks.items, player)) {
            direction.stepBack(&player);
            direction = direction.turn();
        } else {
            const res = seenPositions.get(player);
            if (res) |value| {
                if (value == 100) {
                    // assume we're stuck in a loop
                    return null;
                }
                try seenPositions.put(player, value + 1);
            } else {
                try seenPositions.put(player, 1);
            }
        }
    }

    return seenPositions;
}

fn containsVec(items: []Vec, vec: Vec) bool {
    for (items) |elem| {
        if (elem.x == vec.x and elem.y == vec.y) {
            return true;
        }
    }

    return false;
}

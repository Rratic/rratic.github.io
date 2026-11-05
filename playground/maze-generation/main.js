const WALL = 1;
const SPACE = 0;
const CHUNK_SIZE = 64;

class CRand {
    constructor(seed) {
        this.state = seed >>> 0;
    }

    next() {
        this.state = (Math.imul(this.state, 1103515245) + 12345) >>> 0;
        return (this.state >>> 16) & 32767;
    }

    nextInt(bound) {
        if (bound <= 0) return 0;
        return this.next() % bound;
    }
}

function makeMathRng() {
    return {
        nextInt(bound) {
            if (bound <= 0) return 0;
            return Math.floor(Math.random() * bound);
        },
    };
}

function indexOf(x, y, width) {
    return y * width + x;
}

function generatePrimMaze(width, height, rng) {
    const grid = new Uint8Array(width * height);
    grid.fill(WALL);
    const visited = new Uint8Array(width * height);
    const frontier = [];

    const startX = rng.nextInt(width);
    const startY = rng.nextInt(height);
    const startIndex = indexOf(startX, startY, width);

    grid[startIndex] = SPACE;
    frontier.push(startIndex);

    let endIndex = startIndex;
    const delta = [
        [1, 0],
        [-1, 0],
        [0, 1],
        [0, -1],
    ];

    while (frontier.length > 0) {
        const pick = rng.nextInt(frontier.length);
        const current = frontier[pick];
        frontier[pick] = frontier[frontier.length - 1];
        frontier.pop();

        if (visited[current]) continue;
        visited[current] = 1;

        const cx = current % width;
        const cy = Math.floor(current / width);
        let openNeighbors = 0;

        for (const [dx, dy] of delta) {
            const nx = cx + dx;
            const ny = cy + dy;
            if (nx < 0 || ny < 0 || nx >= width || ny >= height) continue;
            const ni = indexOf(nx, ny, width);
            if (grid[ni] === SPACE) openNeighbors += 1;
        }

        if (openNeighbors <= 1) {
            grid[current] = SPACE;
            endIndex = current;
            for (const [dx, dy] of delta) {
                const nx = cx + dx;
                const ny = cy + dy;
                if (nx < 0 || ny < 0 || nx >= width || ny >= height) continue;
                const ni = indexOf(nx, ny, width);
                if (!visited[ni]) frontier.push(ni);
            }
        }
    }

    return {
        width,
        height,
        grid,
        start: {
            x: startIndex % width,
            y: Math.floor(startIndex / width),
        },
        end: {
            x: endIndex % width,
            y: Math.floor(endIndex / width),
        },
    };
}

function generateChunkMaze(seed, chunkX, chunkY) {
    const width = CHUNK_SIZE;
    const height = CHUNK_SIZE;
    const grid = new Uint8Array(width * height);

    function put(x, y, type) {
        grid[indexOf(x, y, width)] = type;
    }

    const initSeed = ((((chunkX << 15) + chunkY) ^ (seed << 3)) >>> 0);
    const rng = new CRand(initSeed);

    for (let i = 0; i < CHUNK_SIZE; i += 1) {
        put(i, 0, WALL);
        put(0, i, WALL);
    }

    function divide(lx, ly, rx, ry) {
        if (lx > rx || ly > ry) return;

        const x0 = rx - lx;
        const y0 = ry - ly;
        if (x0 <= 0 || y0 <= 0) {
            for (let x = lx; x <= rx; x += 1) {
                for (let y = ly; y <= ry; y += 1) {
                    put(x, y, SPACE);
                }
            }
            return;
        }

        const halfX = x0 >> 1;
        const halfY = y0 >> 1;
        if (halfX === 0 || halfY === 0) {
            for (let x = lx; x <= rx; x += 1) {
                for (let y = ly; y <= ry; y += 1) {
                    put(x, y, SPACE);
                }
            }
            return;
        }

        const mx = lx + 2 * rng.nextInt(halfX) + 1;
        const my = ly + 2 * rng.nextInt(halfY) + 1;

        for (let x = lx; x <= rx; x += 1) put(x, my, WALL);
        for (let y = ly; y <= ry; y += 1) put(mx, y, WALL);

        divide(lx, ly, mx - 1, my - 1);
        divide(lx, my + 1, mx - 1, ry);
        divide(mx + 1, ly, rx, my - 1);
        divide(mx + 1, my + 1, rx, ry);

        const d = rng.nextInt(4);
        const myl = Math.max(1, (my - ly + 1) >> 1);
        const myr = Math.max(1, (ry - my + 1) >> 1);
        const mxl = Math.max(1, (mx - lx + 1) >> 1);
        const mxr = Math.max(1, (rx - mx + 1) >> 1);

        if (d !== 0) put(lx + 2 * rng.nextInt(mxl), my, SPACE);
        if (d !== 1) put(rx - 2 * rng.nextInt(mxr), my, SPACE);
        if (d !== 2) put(mx, ly + 2 * rng.nextInt(myl), SPACE);
        if (d !== 3) put(mx, ry - 2 * rng.nextInt(myr), SPACE);
    }

    divide(1, 1, 63, 63);
    put(2 * rng.nextInt(32) + 1, 0, SPACE);
    put(0, 2 * rng.nextInt(32) + 1, SPACE);

    return { width, height, grid };
}

function drawMaze(canvas, maze) {
    const ctx = canvas.getContext("2d");
    const cell = Math.max(1, Math.floor(640 / Math.max(maze.width, maze.height)));
    const drawWidth = maze.width * cell;
    const drawHeight = maze.height * cell;

    canvas.width = drawWidth;
    canvas.height = drawHeight;

    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, drawWidth, drawHeight);

    for (let y = 0; y < maze.height; y += 1) {
        for (let x = 0; x < maze.width; x += 1) {
            const v = maze.grid[indexOf(x, y, maze.width)];
            ctx.fillStyle = v === WALL ? "#1f2937" : "#ffffff";
            ctx.fillRect(x * cell, y * cell, cell, cell);
        }
    }

    if (maze.start) {
        ctx.fillStyle = "#16a34a";
        ctx.fillRect(maze.start.x * cell, maze.start.y * cell, cell, cell);
    }
    if (maze.end) {
        ctx.fillStyle = "#f59e0b";
        ctx.fillRect(maze.end.x * cell, maze.end.y * cell, cell, cell);
    }
}

function clampInt(value, min, max, fallback) {
    const n = Number(value);
    if (!Number.isFinite(n)) return fallback;
    return Math.max(min, Math.min(max, Math.floor(n)));
}

const canvas = document.getElementById("maze-canvas");
const output = document.getElementById("output");
const selectAlgorithm = document.getElementById("select-algorithm");
const primOptions = document.getElementById("prim-options");
const chunkOptions = document.getElementById("chunk-options");

function updateOptionVisibility() {
    const prim = selectAlgorithm.value === "prim";
    primOptions.classList.toggle("hidden", !prim);
    chunkOptions.classList.toggle("hidden", prim);
}

function generateAndRender() {
    const algorithm = selectAlgorithm.value;
    if (algorithm === "prim") {
        const width = clampInt(document.getElementById("input-width").value, 5, 127, 41);
        const height = clampInt(document.getElementById("input-height").value, 5, 127, 31);
        const maze = generatePrimMaze(width, height, makeMathRng());
        drawMaze(canvas, maze);
        output.innerText = `随机化 Prim：${width}×${height}，起点 (${maze.start.x}, ${maze.start.y})，终点 (${maze.end.x}, ${maze.end.y})`;
        return;
    }

    const seed = clampInt(document.getElementById("input-seed").value, -2147483648, 2147483647, 2026);
    const chunkX = clampInt(document.getElementById("input-chunk-x").value, -1000000, 1000000, 0);
    const chunkY = clampInt(document.getElementById("input-chunk-y").value, -1000000, 1000000, 0);
    const maze = generateChunkMaze(seed >>> 0, chunkX, chunkY);
    drawMaze(canvas, maze);
    output.innerText = `递归分割 Chunk：64×64，seed=${seed}，chunk=(${chunkX}, ${chunkY})`;
}

document.getElementById("button-generate").addEventListener("click", generateAndRender);
selectAlgorithm.addEventListener("change", () => {
    updateOptionVisibility();
    generateAndRender();
});

updateOptionVisibility();
generateAndRender();

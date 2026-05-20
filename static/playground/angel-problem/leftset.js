// 计算边界曲线 κ 的左集 V_κ。
// 思路：以 (-1, 0) 为种子做 BFS，沿格子之间的"未被曲线切断的"边扩散。
// 边的判定：
//   - 若该边在 edgeCount 中出现 1 次（普通边界段），不可穿越；
//   - 若该边在 edgeCount 中出现 0 次且不是虚拟南/北延伸段，可穿越；
//   - 若该边出现 2 次（曲线重叠通过），两侧格均属 V_κ，可穿越；
//   - 虚拟南/北延伸段视作"出现 1 次"。

export function cellKey(x, y) {
    return `${x},${y}`;
}

// 把两个相邻格子 A, B 之间的边换算成端点对 (a, b)。
export function edgeBetweenCells(A, B) {
    if (A.x === B.x) {
        const minY = Math.min(A.y, B.y);
        return {
            a: { x: A.x, y: minY + 1 },
            b: { x: A.x + 1, y: minY + 1 }
        };
    }
    if (A.y === B.y) {
        const minX = Math.min(A.x, B.x);
        return {
            a: { x: minX + 1, y: A.y },
            b: { x: minX + 1, y: A.y + 1 }
        };
    }
    throw new Error("Cells must be 4-neighbors.");
}

const NEIGHBOR_DELTAS = [
    { dx: 1, dy: 0 },
    { dx: -1, dy: 0 },
    { dx: 0, dy: 1 },
    { dx: 0, dy: -1 }
];

function cellInBBox(x, y, bbox) {
    return x >= bbox.minX && x <= bbox.maxX && y >= bbox.minY && y <= bbox.maxY;
}

export function computeLeftSet(curve, bbox) {
    const set = new Set();
    const seed = pickSeed(bbox);
    if (!seed) {
        return set;
    }
    const queue = [seed];
    set.add(cellKey(seed.x, seed.y));

    while (queue.length > 0) {
        const cur = queue.shift();
        for (const { dx, dy } of NEIGHBOR_DELTAS) {
            const nx = cur.x + dx;
            const ny = cur.y + dy;
            if (!cellInBBox(nx, ny, bbox)) {
                continue;
            }
            const nKey = cellKey(nx, ny);
            if (set.has(nKey)) {
                continue;
            }
            const edge = edgeBetweenCells(cur, { x: nx, y: ny });
            const count = curve.getEffectiveEdgeCount(edge.a, edge.b);
            if (count === 1) {
                continue;
            }
            set.add(nKey);
            queue.push({ x: nx, y: ny });
        }
    }
    return set;
}

// 选一个一定在 V_κ 内的种子：x 充分小（x < 0）总能落入初始 V_{λ_0}，而 V_μ ⊇ V_{λ_0} 永远成立。
function pickSeed(bbox) {
    const x = Math.min(-1, bbox.minX);
    const y = clamp(0, bbox.minY, bbox.maxY);
    if (x > bbox.maxX) {
        return null;
    }
    return { x, y };
}

function clamp(value, lo, hi) {
    if (value < lo) return lo;
    if (value > hi) return hi;
    return value;
}

export function countAvoidance(leftSet, bannedHistory) {
    let count = 0;
    for (const key of bannedHistory) {
        if (leftSet.has(key)) {
            count += 1;
        }
    }
    return count;
}

// 解析 edgeKey 字符串得到端点对。
export function parseEdgeKey(key) {
    const sep = key.indexOf("|");
    const left = key.slice(0, sep);
    const right = key.slice(sep + 1);
    const [ax, ay] = left.split(",").map(Number);
    const [bx, by] = right.split(",").map(Number);
    return {
        a: { x: ax, y: ay },
        b: { x: bx, y: by }
    };
}

// 给定单位边的两端点 (a, b)，返回边两侧的两个格子。
export function adjacentCellsOfEdge(a, b) {
    if (a.x === b.x) {
        const minY = Math.min(a.y, b.y);
        return [
            { x: a.x - 1, y: minY },
            { x: a.x, y: minY }
        ];
    }
    const minX = Math.min(a.x, b.x);
    return [
        { x: minX, y: a.y - 1 },
        { x: minX, y: a.y }
    ];
}

function parseCellKey(key) {
    const idx = key.indexOf(",");
    return {
        x: Number(key.slice(0, idx)),
        y: Number(key.slice(idx + 1))
    };
}

// 从 V_κ 出发，在右集可达区域（不穿越 fixed）内做 BFS，返回 dist / parent。
function bfsRightReachable(leftSet, fixedRightCells, bbox) {
    const dist = new Map();
    const parent = new Map();
    const queue = [];

    const trySeed = (cell) => {
        const key = cellKey(cell.x, cell.y);
        if (!cellInBBox(cell.x, cell.y, bbox)) return;
        if (!leftSet.has(key)) return;
        if (dist.has(key)) return;
        dist.set(key, 0);
        queue.push(cell);
    };

    for (const key of leftSet) {
        trySeed(parseCellKey(key));
    }

    let head = 0;
    while (head < queue.length) {
        const cur = queue[head];
        head += 1;
        const curKey = cellKey(cur.x, cur.y);
        const curDist = dist.get(curKey);
        const neighbors = [
            { x: cur.x + 1, y: cur.y },
            { x: cur.x - 1, y: cur.y },
            { x: cur.x, y: cur.y + 1 },
            { x: cur.x, y: cur.y - 1 }
        ];
        for (const n of neighbors) {
            const nKey = cellKey(n.x, n.y);
            if (!cellInBBox(n.x, n.y, bbox)) continue;
            if (leftSet.has(nKey)) continue;
            if (fixedRightCells.has(nKey)) continue;
            if (dist.has(nKey)) continue;
            dist.set(nKey, curDist + 1);
            parent.set(nKey, curKey);
            queue.push(n);
        }
    }

    return { dist, parent };
}

// 为吞并目标格集合 S 计算拓展顺序（含必经的中间格，按离 V 的距离升序）。
export function computeEngulfOrder(targetKeys, leftSet, fixedRightCells, bbox) {
    if (targetKeys.length === 0) {
        return [];
    }

    const { dist, parent } = bfsRightReachable(leftSet, fixedRightCells, bbox);
    const neededKeys = new Set();

    for (const key of targetKeys) {
        if (leftSet.has(key)) continue;
        if (!dist.has(key)) continue;
        let cur = key;
        while (cur !== undefined && !leftSet.has(cur)) {
            neededKeys.add(cur);
            cur = parent.get(cur);
        }
    }

    const ordered = [...neededKeys]
        .map((key) => ({ key, cell: parseCellKey(key), depth: dist.get(key) }))
        .sort((a, b) => {
            if (a.depth !== b.depth) return a.depth - b.depth;
            if (a.cell.y !== b.cell.y) return a.cell.y - b.cell.y;
            return a.cell.x - b.cell.x;
        });

    return ordered.map((item) => item.cell);
}

// 候选格：历史被禁格中，不在 V_λ、不在 past/current 右格里的那些（DFS 只对这些做 include/exclude）。
export function computeBannedCandidates(bannedHistory, leftSet, fixedRightCells, bbox) {
    const { dist } = bfsRightReachable(leftSet, fixedRightCells, bbox);
    const ordered = [];

    for (const key of bannedHistory) {
        if (leftSet.has(key)) continue;
        if (fixedRightCells.has(key)) continue;
        if (!dist.has(key)) continue;
        ordered.push({
            key,
            cell: parseCellKey(key),
            depth: dist.get(key)
        });
    }

    ordered.sort((a, b) => {
        if (a.depth !== b.depth) return a.depth - b.depth;
        if (a.cell.y !== b.cell.y) return a.cell.y - b.cell.y;
        return a.cell.x - b.cell.x;
    });

    return ordered;
}

// 计算 past + current 段的"必须保持在右集"的格子集合。
export function computeFixedRightCells(curve, angelSegIdx, bbox) {
    const fixed = new Set();
    for (let i = 0; i <= angelSegIdx; i += 1) {
        const c = curve.getRightCellAtSegmentIndex(i);
        fixed.add(cellKey(c.x, c.y));
    }
    // 虚拟南延伸的右格也永远在右集中。
    for (let y = bbox.minY; y < 0; y += 1) {
        fixed.add(cellKey(0, y));
    }
    return fixed;
}

// 计算一个能覆盖策略搜索所需的所有相关格子的 bbox。
export function computeRelevantBBox(curve, bannedHistory, angelCell, padding = 3) {
    let minX = -1;
    let maxX = 1;
    let minY = -1;
    let maxY = 1;

    for (const p of curve.points) {
        if (p.x < minX) minX = p.x;
        if (p.x > maxX) maxX = p.x;
        if (p.y < minY) minY = p.y;
        if (p.y > maxY) maxY = p.y;
    }
    for (const key of bannedHistory) {
        const [xStr, yStr] = key.split(",");
        const x = Number(xStr);
        const y = Number(yStr);
        if (x < minX) minX = x;
        if (x > maxX) maxX = x;
        if (y < minY) minY = y;
        if (y > maxY) maxY = y;
    }
    if (angelCell) {
        if (angelCell.x < minX) minX = angelCell.x;
        if (angelCell.x > maxX) maxX = angelCell.x;
        if (angelCell.y < minY) minY = angelCell.y;
        if (angelCell.y > maxY) maxY = angelCell.y;
    }
    minX -= padding;
    maxX += padding;
    minY -= padding;
    maxY += padding;
    return { minX, maxX, minY, maxY };
}

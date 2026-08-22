import { computeLeftSet, cellKey } from "./leftset.js";

/*
边界曲线的有限部分（无限部分为自南向北到达 (0, 0) 与离开 (0, n)）。

起点必须是 (0, 0)
终点必须是 (0, n) 且 n 是非负整数
当 n = 0 时曲线为初始直线，仅由虚拟无限段组成
N = 1 时初始线段为 (0, 0) -> (0, 1)，其右侧格 (0, 0)，左侧格 (-1, 0)
*/

export class BorderCurve {
    constructor() {
        this.points = [{ x: 0, y: 0 }];
        this.edgeCount = new Map();
        this.currLength = 0;
        this.currAvoidance = 0;
        this.prevLength = 0;
        this.prevAvoidance = 0;
    }

    static edgeKey(a, b) {
        if (a.x < b.x || (a.x === b.x && a.y < b.y)) {
            return `${a.x},${a.y}|${b.x},${b.y}`;
        }
        return `${b.x},${b.y}|${a.x},${a.y}`;
    }

    incrementEdge(a, b) {
        const key = BorderCurve.edgeKey(a, b);
        const cur = this.edgeCount.get(key) || 0;
        if (cur >= 2) {
            throw new Error(`Edge already appears 2 times: ${key}`);
        }
        this.edgeCount.set(key, cur + 1);
    }

    decrementEdge(a, b) {
        const key = BorderCurve.edgeKey(a, b);
        const cur = this.edgeCount.get(key) || 0;
        if (cur <= 0) {
            throw new Error(`Cannot decrement edge with count ${cur}: ${key}`);
        }
        if (cur === 1) {
            this.edgeCount.delete(key);
        } else {
            this.edgeCount.set(key, cur - 1);
        }
    }

    getEdgeCount(a, b) {
        return this.edgeCount.get(BorderCurve.edgeKey(a, b)) || 0;
    }

    isVirtualSouthEdge(a, b) {
        if (a.x !== 0 || b.x !== 0) {
            return false;
        }
        if (Math.abs(a.y - b.y) !== 1) {
            return false;
        }
        return Math.min(a.y, b.y) < 0;
    }

    isVirtualNorthEdge(a, b) {
        const end = this.getEndPoint();
        if (a.x !== end.x || b.x !== end.x) {
            return false;
        }
        if (Math.abs(a.y - b.y) !== 1) {
            return false;
        }
        return Math.min(a.y, b.y) >= end.y;
    }

    getEffectiveEdgeCount(a, b) {
        let count = this.getEdgeCount(a, b);
        if (this.isVirtualSouthEdge(a, b) || this.isVirtualNorthEdge(a, b)) {
            count += 1;
        }
        return count;
    }

    getEndPoint() {
        return this.points[this.points.length - 1];
    }

    // 线段数
    count() {
        return this.points.length - 1;
    }

    // “长度”，不重合部分线段数之差
    length() {
        return this.count() - this.getEndPoint().y;
    }

    refreshCurrLength() {
        this.currLength = this.length();
    }

    rotateGeneration() {
        this.prevLength = this.currLength;
        this.prevAvoidance = this.currAvoidance;
        this.currLength = this.length();
    }

    setCurrAvoidance(value) {
        if (!Number.isInteger(value) || value < 0) {
            throw new Error("currAvoidance must be a non-negative integer.");
        }
        this.currAvoidance = value;
    }

    getSegmentByIndex(segmentIndex) {
        if (!Number.isInteger(segmentIndex) || segmentIndex < 0) {
            throw new Error("segmentIndex must be a non-negative integer.");
        }

        const finiteCount = this.count();
        if (segmentIndex < finiteCount) {
            return {
                start: this.points[segmentIndex],
                end: this.points[segmentIndex + 1]
            };
        }

        const extensionIndex = segmentIndex - finiteCount;
        const tail = this.getEndPoint();
        return {
            start: { x: tail.x, y: tail.y + extensionIndex },
            end: { x: tail.x, y: tail.y + extensionIndex + 1 }
        };
    }

    getRightCellAtSegmentIndex(segmentIndex) {
        const segment = this.getSegmentByIndex(segmentIndex);
        return rightCellOfSegment(segment.start, segment.end);
    }

    forEachSegment(visitSegment) {
        for (let i = 1; i < this.points.length; i += 1) {
            visitSegment(this.points[i - 1], this.points[i], i - 1);
        }
    }

    // 在 segmentIndex 处把虚拟北延伸的段落物化为有限段，使得 segmentIndex < count()。
    materializeNorthUpTo(segmentIndex) {
        const targetCount = segmentIndex + 1;
        while (this.count() < targetCount) {
            const tail = this.getEndPoint();
            const next = { x: tail.x, y: tail.y + 1 };
            this.incrementEdge(tail, next);
            this.points.push(next);
        }
    }

    // 在指定段做“拓展”：把该段替换为绕其右格反向方向的三条段。
    // 调用方需保证该段确实可拓展（右格在 V_κ 之外）。
    extendSegment(segmentIndex) {
        if (segmentIndex >= this.count()) {
            this.materializeNorthUpTo(segmentIndex);
        }
        const A = this.points[segmentIndex];
        const B = this.points[segmentIndex + 1];
        const detour = computeDetour(A, B);
        const [P, Q] = detour;

        this.decrementEdge(A, B);
        this.incrementEdge(A, P);
        this.incrementEdge(P, Q);
        this.incrementEdge(Q, B);
        this.points.splice(segmentIndex + 1, 0, P, Q);
        this.refreshCurrLength();
    }

    // 若 points[pointIndex-1] === points[pointIndex+1]，连续两段方向相反，删除它们。
    tryContractAt(pointIndex) {
        if (pointIndex < 1 || pointIndex >= this.points.length - 1) {
            return false;
        }
        const before = this.points[pointIndex - 1];
        const after = this.points[pointIndex + 1];
        if (before.x !== after.x || before.y !== after.y) {
            return false;
        }
        const mid = this.points[pointIndex];
        this.decrementEdge(before, mid);
        this.decrementEdge(mid, after);
        this.points.splice(pointIndex, 2);
        this.refreshCurrLength();
        return true;
    }

    // 若曲线最后一段方向为 S（直接抵到 endpoint 上方又掉回 endpoint），
    // 该段与紧接其后的虚拟北延伸第一段在物理上重合方向相反，可吸收：
    // 删除最后一个有限端点，使 endpoint 北移一格。
    absorbTrailingNorth() {
        if (this.points.length < 2) {
            return false;
        }
        const last = this.points[this.points.length - 1];
        const prev = this.points[this.points.length - 2];
        if (prev.x !== last.x || prev.y !== last.y + 1) {
            return false;
        }
        // 仅当此边在曲线里恰好出现 1 次时直接吸收，避免 count=2 时破坏拓扑。
        if (this.getEdgeCount(prev, last) !== 1) {
            return false;
        }
        this.decrementEdge(prev, last);
        this.points.pop();
        this.refreshCurrLength();
        return true;
    }

    // 把所有可消除的连续反向对消除，直至曲线达到规范形式。
    fullyContract() {
        let changed = true;
        while (changed) {
            changed = false;
            for (let i = 1; i < this.points.length - 1; i += 1) {
                if (this.tryContractAt(i)) {
                    changed = true;
                    break;
                }
            }
            if (changed) continue;
            if (this.absorbTrailingNorth()) {
                changed = true;
            }
        }
    }

    snapshot() {
        return {
            points: this.points.map(p => ({ x: p.x, y: p.y })),
            edgeCount: new Map(this.edgeCount),
            currLength: this.currLength,
            currAvoidance: this.currAvoidance,
            prevLength: this.prevLength,
            prevAvoidance: this.prevAvoidance
        };
    }

    restore(snap) {
        this.points = snap.points.map(p => ({ x: p.x, y: p.y }));
        this.edgeCount = new Map(snap.edgeCount);
        this.currLength = snap.currLength;
        this.currAvoidance = snap.currAvoidance;
        this.prevLength = snap.prevLength;
        this.prevAvoidance = snap.prevAvoidance;
    }

    clone() {
        const next = new BorderCurve();
        next.restore(this.snapshot());
        return next;
    }

    replaceWith(other) {
        this.restore(other.snapshot());
    }

    reset() {
        this.points = [{ x: 0, y: 0 }];
        this.edgeCount = new Map();
        this.currLength = 0;
        this.currAvoidance = 0;
        this.prevLength = 0;
        this.prevAvoidance = 0;
    }
}

export function segmentDirection(start, end) {
    const dx = end.x - start.x;
    const dy = end.y - start.y;
    if (dx === 0 && dy === 1) return "N";
    if (dx === 1 && dy === 0) return "E";
    if (dx === 0 && dy === -1) return "S";
    if (dx === -1 && dy === 0) return "W";
    return null;
}

/** 沿曲线走向的顺时针拐角：N→E→S→W→N */
export function isClockwiseTurn(dirIn, dirOut) {
    const next = { N: "E", E: "S", S: "W", W: "N" };
    return next[dirIn] === dirOut;
}

/** 段 segIdx 的终点 points[segIdx+1] 处是否为顺时针拐角（需存在下一段） */
export function isClockwiseCornerAtSegmentEnd(curve, segIdx) {
    if (segIdx < 0 || segIdx + 2 >= curve.points.length) {
        return false;
    }
    const a = curve.points[segIdx];
    const b = curve.points[segIdx + 1];
    const c = curve.points[segIdx + 2];
    const dIn = segmentDirection(a, b);
    const dOut = segmentDirection(b, c);
    if (!dIn || !dOut) return false;
    return isClockwiseTurn(dIn, dOut);
}

function rightCellOfSegment(start, end) {
    const dx = end.x - start.x;
    const dy = end.y - start.y;

    if (dx === 0 && dy === 1) {
        return { x: start.x, y: start.y };
    }
    if (dx === 1 && dy === 0) {
        return { x: start.x, y: start.y - 1 };
    }
    if (dx === 0 && dy === -1) {
        return { x: start.x - 1, y: start.y - 1 };
    }
    if (dx === -1 && dy === 0) {
        return { x: start.x - 1, y: start.y };
    }

    throw new Error("Segment must be axis-aligned with unit length.");
}

// 给定一条 A -> B 段，计算其拓展后的两个中间顶点 P, Q。
// 三条新段方向依次为：右转(d), d, 左转(d)。
function computeDetour(A, B) {
    const dx = B.x - A.x;
    const dy = B.y - A.y;
    if (dx === 0 && dy === 1) {
        return [
            { x: A.x + 1, y: A.y },
            { x: A.x + 1, y: A.y + 1 }
        ];
    }
    if (dx === 1 && dy === 0) {
        return [
            { x: A.x, y: A.y - 1 },
            { x: A.x + 1, y: A.y - 1 }
        ];
    }
    if (dx === 0 && dy === -1) {
        return [
            { x: A.x - 1, y: A.y },
            { x: A.x - 1, y: A.y - 1 }
        ];
    }
    if (dx === -1 && dy === 0) {
        return [
            { x: A.x, y: A.y + 1 },
            { x: A.x - 1, y: A.y + 1 }
        ];
    }
    throw new Error("Segment must be axis-aligned with unit length.");
}

function parseCellKey(key) {
    const idx = key.indexOf(",");
    return {
        x: Number(key.slice(0, idx)),
        y: Number(key.slice(idx + 1))
    };
}

function maxFutureSegmentIndex(curve, angelSegmentIndex, bbox) {
    const end = curve.getEndPoint();
    const finiteCount = curve.count();
    const virtualNorth = Math.max(0, bbox.maxY - end.y);
    return angelSegmentIndex + finiteCount + virtualNorth + 8;
}

function isTargetSetEngulfed(curve, targetKeys, bbox) {
    const leftSet = computeLeftSet(curve, bbox);
    for (const key of targetKeys) {
        if (!leftSet.has(key)) {
            return false;
        }
    }
    return true;
}

/**
 * 将 targetKeys 全部吞入 V_μ：反复扫描未来段，对右格在 target 且不在 V 的段做拓展+收缩。
 * 返回 { curve, complete }，仅 complete === true 时方可作为 P_i 候选。
 */
export function buildCurveForTargetKeys(baseCurve, targetKeys, angelSegmentIndex, bbox) {
    const work = baseCurve.clone();
    if (targetKeys.size === 0) {
        return { curve: work, complete: true };
    }

    const maxSeg = maxFutureSegmentIndex(work, angelSegmentIndex, bbox);
    const maxRounds = Math.max(targetKeys.size * 8, 64);

    for (let round = 0; round < maxRounds; round += 1) {
        if (isTargetSetEngulfed(work, targetKeys, bbox)) {
            return { curve: work, complete: true };
        }

        let progressed = false;
        for (let segIdx = angelSegmentIndex + 1; segIdx <= maxSeg; segIdx += 1) {
            const right = work.getRightCellAtSegmentIndex(segIdx);
            const key = cellKey(right.x, right.y);
            if (!targetKeys.has(key)) {
                continue;
            }
            const leftSet = computeLeftSet(work, bbox);
            if (leftSet.has(key)) {
                continue;
            }
            work.extendSegment(segIdx);
            work.fullyContract();
            progressed = true;
            break;
        }

        if (!progressed) {
            return { curve: work, complete: false };
        }
    }

    return {
        curve: work,
        complete: isTargetSetEngulfed(work, targetKeys, bbox)
    };
}

/** @deprecated 使用 buildCurveForTargetKeys */
export function buildCurveByExpansion(baseCurve, cells, angelSegmentIndex, bbox) {
    const keys = new Set(cells.map((c) => cellKey(c.x, c.y)));
    return buildCurveForTargetKeys(baseCurve, keys, angelSegmentIndex, bbox).curve;
}

// 在 angelSegmentIndex 之后的段落里寻找右格等于 targetCell 的段索引。
// 同时扫描有限段以及（如果 targetCell 在虚拟北延伸射线上）相应的虚拟段索引。
export function findExtendableSegmentForCell(curve, targetCell, angelSegmentIndex) {
    const finiteCount = curve.count();
    for (let i = angelSegmentIndex + 1; i < finiteCount; i += 1) {
        const cell = curve.getRightCellAtSegmentIndex(i);
        if (cell.x === targetCell.x && cell.y === targetCell.y) {
            return i;
        }
    }
    const end = curve.getEndPoint();
    if (targetCell.x === end.x && targetCell.y >= end.y) {
        const candidate = finiteCount + (targetCell.y - end.y);
        if (candidate > angelSegmentIndex) {
            return candidate;
        }
    }
    return -1;
}

export function createFrameLineOverlay(path, color) {
    return ({ ctx, renderer, visibleRange }) => {
        const points = path.points;
        const end = path.getEndPoint();
        const start = points[0];

        ctx.save();
        ctx.strokeStyle = color;
        ctx.lineWidth = 4;
        ctx.lineCap = "round";
        ctx.lineJoin = "round";

        if (points.length >= 2) {
            ctx.beginPath();
            const first = renderer.worldToScreen(points[0].x, points[0].y);
            ctx.moveTo(first.x + 0.5, first.y + 0.5);
            for (let i = 1; i < points.length; i += 1) {
                const pos = renderer.worldToScreen(points[i].x, points[i].y);
                ctx.lineTo(pos.x + 0.5, pos.y + 0.5);
            }
            ctx.stroke();
        }

        const southLimitY = visibleRange.minY - 2;
        const northLimitY = visibleRange.maxY + 2;

        const southStart = renderer.worldToScreen(start.x, start.y);
        const southEnd = renderer.worldToScreen(start.x, southLimitY);
        const northStart = renderer.worldToScreen(end.x, end.y);
        const northEnd = renderer.worldToScreen(end.x, northLimitY);

        ctx.beginPath();
        ctx.moveTo(southStart.x + 0.5, southStart.y + 0.5);
        ctx.lineTo(southEnd.x + 0.5, southEnd.y + 0.5);
        ctx.moveTo(northStart.x + 0.5, northStart.y + 0.5);
        ctx.lineTo(northEnd.x + 0.5, northEnd.y + 0.5);
        ctx.stroke();
        ctx.restore();
    };
}

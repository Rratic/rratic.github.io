import { CellState } from "./grids.js";
import { cellKey, computeLeftSet } from "./leftset.js";
import { isClockwiseCornerAtSegmentEnd } from "./curve.js";

function maxFutureSegmentIndex(curve, angelSegmentIndex, bbox) {
    const end = curve.getEndPoint();
    const finiteCount = curve.count();
    const virtualNorth = Math.max(0, bbox.maxY - end.y);
    return angelSegmentIndex + finiteCount + virtualNorth + 4;
}

function curveSignature(curve) {
    return curve.points.map((p) => `${p.x},${p.y}`).join("|");
}

function leftSetSignature(leftSet) {
    const keys = [...leftSet];
    keys.sort();
    return keys.join(";");
}

/** 正文拓展：右格 q ∉ V_κ。SPACE 若与禁格或已入 V 的规避格相邻（含 pocket）则可拓展。 */
function isExtendableRightCell(right, map, leftSet) {
    const key = cellKey(right.x, right.y);
    if (leftSet.has(key)) return false;
    if (map.getCellState(right.x, right.y) === CellState.BANNED) return true;
    if (map.getCellState(right.x, right.y) !== CellState.SPACE) return false;
    for (const { dx, dy } of [
        { dx: 1, dy: 0 },
        { dx: -1, dy: 0 },
        { dx: 0, dy: 1 },
        { dx: 0, dy: -1 }
    ]) {
        const nx = right.x + dx;
        const ny = right.y + dy;
        const nKey = cellKey(nx, ny);
        const state = map.getCellState(nx, ny);
        if (state === CellState.BANNED && !leftSet.has(nKey)) return true;
        if (leftSet.has(nKey) && state === CellState.AVOIDED) return true;
    }
    return false;
}

function tryExtendAt(curve, segIdx) {
    const next = curve.clone();
    next.extendSegment(segIdx);
    next.fullyContract();
    return next;
}

function collectExtensionIndices(curve, angelSegIdx, fixedRightCells, bbox, map) {
    const leftSet = computeLeftSet(curve, bbox);
    const maxSeg = maxFutureSegmentIndex(curve, angelSegIdx, bbox);
    const clockwise = [];
    const other = [];

    for (let segIdx = angelSegIdx + 1; segIdx <= maxSeg; segIdx += 1) {
        const right = curve.getRightCellAtSegmentIndex(segIdx);
        const key = cellKey(right.x, right.y);
        if (fixedRightCells.has(key)) continue;
        if (!isExtendableRightCell(right, map, leftSet)) continue;

        if (segIdx + 1 < curve.points.length && isClockwiseCornerAtSegmentEnd(curve, segIdx)) {
            clockwise.push(segIdx);
        } else {
            other.push(segIdx);
        }
    }

    return { clockwise, other };
}

/**
 * 顺时针拐角外凸：可连续多次，每凸一处后 fullyContract 并重新扫描，直至无可凸拐角。
 */
export function applyGreedyClockwiseBulges(curve, angelSegIdx, fixedRightCells, bbox, map) {
    const work = curve.clone();
    work.fullyContract();

    while (true) {
        const { clockwise } = collectExtensionIndices(
            work,
            angelSegIdx,
            fixedRightCells,
            bbox,
            map
        );
        if (clockwise.length === 0) {
            break;
        }

        let progressed = false;
        const sorted = [...clockwise].sort((a, b) => b - a);
        for (const segIdx of sorted) {
            if (!isClockwiseCornerAtSegmentEnd(work, segIdx)) {
                continue;
            }
            const leftSet = computeLeftSet(work, bbox);
            const right = work.getRightCellAtSegmentIndex(segIdx);
            const key = cellKey(right.x, right.y);
            if (fixedRightCells.has(key)) {
                continue;
            }
            if (!isExtendableRightCell(right, map, leftSet)) {
                continue;
            }
            work.extendSegment(segIdx);
            work.fullyContract();
            progressed = true;
            break;
        }

        if (!progressed) {
            break;
        }
    }

    return work;
}

function generateMoves(curve, angelSegIdx, fixedRightCells, bbox, map) {
    const moves = [];

    const fullyBulged = applyGreedyClockwiseBulges(
        curve,
        angelSegIdx,
        fixedRightCells,
        bbox,
        map
    );
    if (curveSignature(fullyBulged) !== curveSignature(curve)) {
        moves.push(fullyBulged);
    }

    const { clockwise, other } = collectExtensionIndices(
        curve,
        angelSegIdx,
        fixedRightCells,
        bbox,
        map
    );

    for (const segIdx of clockwise) {
        moves.push(tryExtendAt(curve, segIdx));
    }
    for (const segIdx of other) {
        moves.push(tryExtendAt(curve, segIdx));
    }

    return generateContractions(curve, angelSegIdx, moves);
}

function generateContractions(curve, angelSegIdx, moves) {
    const minContractPoint = angelSegIdx + 2;
    for (let pi = minContractPoint; pi < curve.points.length - 1; pi += 1) {
        const next = curve.clone();
        if (!next.tryContractAt(pi)) continue;
        next.fullyContract();
        moves.push(next);
    }

    const tail = curve.clone();
    if (tail.absorbTrailingNorth()) {
        tail.fullyContract();
        moves.push(tail);
    }

    return moves;
}

/**
 * BFS 枚举 P_i：由有限次拓展/收缩得到、且过去段+当前段与 λ_{i-1} 一致的全部规范形曲线。
 */
export function enumerateDescendantCurves({
    baseCurve,
    angelSegIdx,
    fixedRightCells,
    bbox,
    map,
    evaluateCurve,
    pastCurrentSegmentsMatch,
    maxStates = 400000
}) {
    const feasible = [];
    const visitedCurve = new Set();
    const bestLByLeft = new Map();
    const queue = [];

    const start = applyGreedyClockwiseBulges(
        baseCurve,
        angelSegIdx,
        fixedRightCells,
        bbox,
        map
    );
    const startSig = curveSignature(start);
    visitedCurve.add(startSig);
    queue.push(start);

    let expanded = 0;
    let truncated = false;

    while (queue.length > 0 && expanded < maxStates) {
        const curve = queue.shift();
        expanded += 1;

        if (!pastCurrentSegmentsMatch(curve, baseCurve, angelSegIdx)) {
            continue;
        }

        feasible.push(evaluateCurve(curve, map, bbox));

        const bulged = applyGreedyClockwiseBulges(
            curve,
            angelSegIdx,
            fixedRightCells,
            bbox,
            map
        );
        enqueue(bulged);

        for (const next of generateMoves(curve, angelSegIdx, fixedRightCells, bbox, map)) {
            enqueue(next);
        }
    }

    if (queue.length > 0) {
        truncated = true;
    }

    return { feasible, truncated };

    function enqueue(next) {
        if (!pastCurrentSegmentsMatch(next, baseCurve, angelSegIdx)) {
            return;
        }
        const sig = curveSignature(next);
        if (visitedCurve.has(sig)) {
            return;
        }

        const leftSet = computeLeftSet(next, bbox);
        const leftSig = leftSetSignature(leftSet);
        const L = next.length();
        const prevBestL = bestLByLeft.get(leftSig);
        if (prevBestL !== undefined && L >= prevBestL) {
            return;
        }
        bestLByLeft.set(leftSig, L);

        visitedCurve.add(sig);
        queue.push(next);
    }
}

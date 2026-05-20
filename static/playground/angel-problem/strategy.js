import { buildCurveByExpansion } from "./curve.js";
import {
    computeLeftSet,
    countAvoidance,
    computeRelevantBBox,
    computeFixedRightCells,
    computeBannedCandidates,
    computeEngulfOrder
} from "./leftset.js";

const MAX_DFS_NODES = 200000;

function isBetterChoice(next, best, newWallKey) {
    if (next.f < best.f) return true;
    if (next.f > best.f) return false;

    if (newWallKey) {
        if (next.hasNewWall && !best.hasNewWall) return true;
        if (!next.hasNewWall && best.hasNewWall) return false;
    }

    // f 相同时优先吞并更多历史被禁格（主动规避）；若仍相同则保持更短曲线。
    if (next.n > best.n) return true;
    if (next.n < best.n) return false;
    return next.segmentCount < best.segmentCount;
}

function evaluateCurve(curve, bannedHistory, bbox, newWallKey) {
    const leftSet = computeLeftSet(curve, bbox);
    const n = countAvoidance(leftSet, bannedHistory);
    const L = curve.length();
    const f = L - 2 * n;
    return {
        f,
        n,
        curve,
        hasNewWall: newWallKey ? leftSet.has(newWallKey) : false,
        segmentCount: curve.count()
    };
}

// 在一轮魔鬼操作后，依据 λ_{i-1} 搜出 λ_i ∈ R_i（按 L-2n 最小，再按是否吞并本轮新墙、n 最大）。
export function planNextCurve({ navigation, angel, bannedHistory, newWallKey = null }) {
    const angelSegIdx = angel.segmentIndex;
    const angelCell = angel.getCell();
    const baseCurve = navigation.clone();

    const bbox = computeRelevantBBox(baseCurve, bannedHistory, angelCell, 4);
    const Vlambda = computeLeftSet(baseCurve, bbox);
    const n_lambda_i = countAvoidance(Vlambda, bannedHistory);
    const L_lambda = baseCurve.length();
    const f_baseline = L_lambda - 2 * n_lambda_i;

    const fixedRightCells = computeFixedRightCells(baseCurve, angelSegIdx, bbox);

    const candidates = computeBannedCandidates(
        bannedHistory,
        Vlambda,
        fixedRightCells,
        bbox
    );

    let best = evaluateCurve(baseCurve, bannedHistory, bbox, newWallKey);
    best.f = f_baseline;
    best.n = n_lambda_i;

    const includedBanned = new Set();
    let nodeCount = 0;
    let aborted = false;

    function dfs(idx) {
        if (aborted) return;
        nodeCount += 1;
        if (nodeCount > MAX_DFS_NODES) {
            aborted = true;
            return;
        }

        const targetKeys = [...includedBanned];
        const engulfOrder = computeEngulfOrder(
            targetKeys,
            Vlambda,
            fixedRightCells,
            bbox
        );

        if (engulfOrder.length > 0) {
            try {
                const expanded = buildCurveByExpansion(
                    baseCurve,
                    engulfOrder,
                    angelSegIdx
                );
                const candidate = evaluateCurve(
                    expanded,
                    bannedHistory,
                    bbox,
                    newWallKey
                );
                if (isBetterChoice(candidate, best, newWallKey)) {
                    best = candidate;
                }
            } catch {
                // 当前子集不可通过有限次拓展实现，跳过。
            }
        }

        const remaining = candidates.length - idx;
        if (best.f - 4 * remaining > best.f) {
            // 乐观剪枝：剩余被禁格即使全部吞并也无法让 f 再降 4 以上。
        }
        if (best.f - 4 * remaining > f_baseline && best.f === f_baseline) {
            // 已等于 baseline 且乐观界无法更优时可提前剪枝（保守保留搜索）。
        }

        if (idx >= candidates.length) return;

        const candidate = candidates[idx];

        dfs(idx + 1);
        if (aborted) return;

        includedBanned.add(candidate.key);
        dfs(idx + 1);
        includedBanned.delete(candidate.key);
    }

    dfs(0);

    const chosen = best.curve.clone();
    chosen.prevLength = navigation.currLength;
    chosen.prevAvoidance = navigation.currAvoidance;
    chosen.currLength = chosen.length();
    chosen.setCurrAvoidance(best.n);
    return chosen;
}

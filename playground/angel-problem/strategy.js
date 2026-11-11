import {
    computeLeftSet,
    countAvoidance,
    computeRelevantBBox,
    computeFixedRightCells
} from "./leftset.js";
import { enumerateDescendantCurves } from "./descendants.js";

const MAX_DESCENDANT_STATES = 400000;

function evaluateCurve(curve, map, bbox) {
    const leftSet = computeLeftSet(curve, bbox);
    const n = countAvoidance(leftSet, map);
    const L = curve.length();
    const f = L - 2 * n;
    return { f, n, L, curve, segmentCount: curve.count() };
}

function pastCurrentSegmentsMatch(mu, lambda, angelSegIdx) {
    for (let i = 0; i <= angelSegIdx; i += 1) {
        const a = mu.getSegmentByIndex(i);
        const b = lambda.getSegmentByIndex(i);
        if (
            a.start.x !== b.start.x ||
            a.start.y !== b.start.y ||
            a.end.x !== b.end.x ||
            a.end.y !== b.end.y
        ) {
            return false;
        }
    }
    return true;
}

/** Q_i：f = L - 2n 最小；R_i：n 最大；再比段数少者优先 */
function selectFromQiRi(feasible) {
    if (feasible.length === 0) {
        return null;
    }
    const fMin = Math.min(...feasible.map((item) => item.f));
    const tierQ = feasible.filter((item) => item.f === fMin);
    const nMax = Math.max(...tierQ.map((item) => item.n));
    const tierR = tierQ.filter((item) => item.n === nMax);
    tierR.sort((a, b) => a.segmentCount - b.segmentCount);
    return tierR[0];
}

export function planNextCurve({ navigation, angel, map }) {
    const angelSegIdx = angel.segmentIndex;
    const angelCell = angel.getCell();
    const baseCurve = navigation.clone();
    baseCurve.fullyContract();

    const bbox = computeRelevantBBox(baseCurve, map, angelCell, 4);
    const Vlambda = computeLeftSet(baseCurve, bbox);
    const fixedRightCells = computeFixedRightCells(baseCurve, angelSegIdx, bbox);

    const { feasible, truncated } = enumerateDescendantCurves({
        baseCurve,
        angelSegIdx,
        fixedRightCells,
        bbox,
        map,
        evaluateCurve,
        pastCurrentSegmentsMatch,
        maxStates: MAX_DESCENDANT_STATES
    });

    const chosen = selectFromQiRi(feasible);
    const chosenCurve = chosen ? chosen.curve.clone() : baseCurve.clone();
    const nOut = chosen ? chosen.n : countAvoidance(Vlambda, map);

    chosenCurve.prevLength = navigation.currLength;
    chosenCurve.prevAvoidance = navigation.currAvoidance;
    chosenCurve.currLength = chosenCurve.length();
    chosenCurve.setCurrAvoidance(nOut);

    if (truncated && typeof console !== "undefined" && console.warn) {
        console.warn(
            "[angel-problem] 后代枚举达到上限，Q_i 仅在已访问子集中取最优"
        );
    }

    return chosenCurve;
}

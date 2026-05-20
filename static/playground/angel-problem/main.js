import { CellState, ChunkedGridMap } from "./grids.js";
import { InfiniteGridRenderer } from "./renderer.js";
import { BorderCurve, createFrameLineOverlay } from "./curve.js";
import { Angel } from "./angel.js";
import { planNextCurve } from "./strategy.js";
import { cellKey, computeLeftSet } from "./leftset.js";

const CLICK_MOVE_THRESHOLD = 4;

const canvas = document.getElementById("viewport");
const clearButton = document.getElementById("button-clear");
const infoButton = document.getElementById("button-info");
const infoPanel = document.getElementById("info-panel");
const statusL = document.getElementById("status-l");
const statusN = document.getElementById("status-n");
const statusRound = document.getElementById("status-round");

const map = new ChunkedGridMap(32);
const renderer = new InfiniteGridRenderer(canvas, map, 60);

const navigation = new BorderCurve();
const angel = new Angel(navigation);
const bannedHistory = new Set();
let roundNumber = 0;

globalThis.navigation = navigation;
globalThis.angel = angel;
globalThis.bannedHistory = bannedHistory;

renderer.addOverlayDrawer(({ ctx, renderer: view, visibleRange }) => {
    drawAvoidanceCells(ctx, view, visibleRange);
});
renderer.addOverlayDrawer(createFrameLineOverlay(navigation, "#0a84ff"));
renderer.addOverlayDrawer(({ ctx, renderer: view, visibleRange }) => {
    angel.drawVisitedSteps(ctx, view, visibleRange);
});
renderer.addOverlayDrawer(({ ctx, renderer: view }) => {
    angel.draw(ctx, view);
});

function drawAvoidanceCells(ctx, view, visibleRange) {
    const bbox = {
        minX: visibleRange.minX - 1,
        maxX: visibleRange.maxX + 1,
        minY: visibleRange.minY - 1,
        maxY: visibleRange.maxY + 1
    };
    const leftSet = computeLeftSet(navigation, bbox);
    ctx.save();
    ctx.fillStyle = "rgba(10, 132, 255, 0.10)";
    for (const key of leftSet) {
        const idx = key.indexOf(",");
        const x = Number(key.slice(0, idx));
        const y = Number(key.slice(idx + 1));
        if (x < visibleRange.minX || x > visibleRange.maxX) continue;
        if (y < visibleRange.minY || y > visibleRange.maxY) continue;
        const pos = view.worldToScreen(x, y);
        ctx.fillRect(pos.x, pos.y - view.cellSize, view.cellSize, view.cellSize);
    }
    ctx.restore();
}

function updateStatusPanel() {
    if (statusL) statusL.textContent = String(navigation.currLength);
    if (statusN) statusN.textContent = String(navigation.currAvoidance);
    if (statusRound) statusRound.textContent = String(roundNumber);
}

updateStatusPanel();

const pointerState = {
    id: null,
    isDown: false,
    moved: false,
    downX: 0,
    downY: 0,
    lastX: 0,
    lastY: 0
};

function isSameCell(a, b) {
    return a.x === b.x && a.y === b.y;
}

function onPointerDown(event) {
    pointerState.id = event.pointerId;
    pointerState.isDown = true;
    pointerState.moved = false;
    pointerState.downX = event.clientX;
    pointerState.downY = event.clientY;
    pointerState.lastX = event.clientX;
    pointerState.lastY = event.clientY;

    canvas.setPointerCapture(event.pointerId);
    renderer.setDraggingState(true);
}

function onPointerMove(event) {
    if (!pointerState.isDown || pointerState.id !== event.pointerId) {
        return;
    }

    const dx = event.clientX - pointerState.lastX;
    const dy = event.clientY - pointerState.lastY;
    const totalDx = event.clientX - pointerState.downX;
    const totalDy = event.clientY - pointerState.downY;

    if (!pointerState.moved) {
        const movedDistance = Math.hypot(totalDx, totalDy);
        if (movedDistance > CLICK_MOVE_THRESHOLD) {
            pointerState.moved = true;
        } else {
            return;
        }
    }

    pointerState.lastX = event.clientX;
    pointerState.lastY = event.clientY;
    renderer.panBy(dx, dy);
}

function onPointerUp(event) {
    if (!pointerState.isDown || pointerState.id !== event.pointerId) {
        return;
    }

    if (!pointerState.moved) {
        const cell = renderer.screenToCell(event.clientX, event.clientY);
        const angelCell = angel.getCell();
        const currentState = map.getCellState(cell.x, cell.y);

        const canPlaceWall = currentState === CellState.SPACE && !isSameCell(cell, angelCell);
        if (canPlaceWall) {
            performDevilMove(cell);
        }
    }

    pointerState.isDown = false;
    pointerState.id = null;
    canvas.releasePointerCapture(event.pointerId);
    renderer.setDraggingState(false);
    renderer.render();
}

function performDevilMove(cell) {
    map.setCellState(cell.x, cell.y, CellState.WALL);
    bannedHistory.add(cellKey(cell.x, cell.y));
    roundNumber += 1;

    const newCurve = planNextCurve({
        navigation,
        angel,
        bannedHistory,
        newWallKey: cellKey(cell.x, cell.y)
    });
    navigation.replaceWith(newCurve);
    angel.moveBy(2);
    updateStatusPanel();
}

function resetGame() {
    map.clear();
    bannedHistory.clear();
    roundNumber = 0;
    navigation.reset();
    angel.segmentIndex = 0;
    angel.moveStep = 0;
    angel.visitedCellSteps = new Map();
    angel.recordCurrentVisit();
    updateStatusPanel();
}

window.addEventListener("resize", () => renderer.resize());
canvas.addEventListener("pointerdown", onPointerDown);
canvas.addEventListener("pointermove", onPointerMove);
canvas.addEventListener("pointerup", onPointerUp);
canvas.addEventListener("pointercancel", onPointerUp);
clearButton.addEventListener("click", () => {
    resetGame();
    renderer.render();
});
infoButton.addEventListener("click", () => {
    infoPanel.classList.toggle("hidden");
});

renderer.resize();
renderer.centerCell(0, 0);
renderer.render();

import { CellState } from "./grids.js";

const DEFAULT_CELL_SIZE = 60;
const GRID_COLOR = "#dcdcdc";
const WALL_COLOR = "#111111";
const SPACE_COLOR = "#ffffff";

export class InfiniteGridRenderer {
    constructor(canvas, map, cellSize = DEFAULT_CELL_SIZE) {
        this.canvas = canvas;
        this.ctx = canvas.getContext("2d");
        this.map = map;
        this.cellSize = cellSize;
        this.offsetX = 0;
        this.offsetY = 0;
        this.overlayDrawers = [];
    }

    resize(width = window.innerWidth, height = window.innerHeight) {
        this.canvas.width = width;
        this.canvas.height = height;
        this.render();
    }

    panBy(dx, dy) {
        this.offsetX += dx;
        this.offsetY += dy;
        this.render();
    }

    screenToCell(screenX, screenY) {
        return {
            x: Math.floor((screenX - this.offsetX) / this.cellSize),
            y: Math.floor((this.offsetY - screenY) / this.cellSize)
        };
    }

    worldToScreen(cellX, cellY) {
        return {
            x: cellX * this.cellSize + this.offsetX,
            y: this.offsetY - cellY * this.cellSize
        };
    }

    setDraggingState(isDragging) {
        this.canvas.classList.toggle("dragging", isDragging);
    }

    addOverlayDrawer(drawer) {
        this.overlayDrawers.push(drawer);
    }

    centerCell(cellX, cellY) {
        this.offsetX = this.canvas.width * 0.5 - cellX * this.cellSize;
        this.offsetY = this.canvas.height * 0.5 + cellY * this.cellSize;
    }

    visibleCellRange() {
        const topLeft = this.screenToCell(0, 0);
        const bottomRight = this.screenToCell(this.canvas.width, this.canvas.height);
        return {
            minX: Math.min(topLeft.x, bottomRight.x) - 1,
            maxX: Math.max(topLeft.x, bottomRight.x) + 1,
            minY: Math.min(topLeft.y, bottomRight.y) - 1,
            maxY: Math.max(topLeft.y, bottomRight.y) + 1
        };
    }

    drawBackground() {
        this.ctx.fillStyle = SPACE_COLOR;
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
    }

    drawWallMarks() {
        const range = this.visibleCellRange();
        const inset = this.cellSize * 0.24;
        this.ctx.strokeStyle = WALL_COLOR;
        this.ctx.lineWidth = Math.max(2, this.cellSize * 0.08);
        this.ctx.lineCap = "round";
        this.ctx.beginPath();

        this.map.forEachCellWithStateInRange(range, CellState.WALL, (cellX, cellY) => {
            const pos = this.worldToScreen(cellX, cellY);
            const cellTop = pos.y - this.cellSize;
            const left = pos.x + inset;
            const right = pos.x + this.cellSize - inset;
            const top = cellTop + inset;
            const bottom = cellTop + this.cellSize - inset;
            this.ctx.moveTo(left, top);
            this.ctx.lineTo(right, bottom);
            this.ctx.moveTo(right, top);
            this.ctx.lineTo(left, bottom);
        });
        this.ctx.stroke();
    }

    drawGridLines() {
        this.ctx.strokeStyle = GRID_COLOR;
        this.ctx.lineWidth = 2;
        this.ctx.beginPath();

        let startX = this.offsetX % this.cellSize;
        if (startX < 0) {
            startX += this.cellSize;
        }
        for (let x = startX; x <= this.canvas.width; x += this.cellSize) {
            this.ctx.moveTo(x + 0.5, 0);
            this.ctx.lineTo(x + 0.5, this.canvas.height);
        }

        let startY = this.offsetY % this.cellSize;
        if (startY < 0) {
            startY += this.cellSize;
        }
        for (let y = startY; y <= this.canvas.height; y += this.cellSize) {
            this.ctx.moveTo(0, y + 0.5);
            this.ctx.lineTo(this.canvas.width, y + 0.5);
        }

        this.ctx.stroke();
    }

    drawOverlays() {
        const visibleRange = this.visibleCellRange();
        for (const drawOverlay of this.overlayDrawers) {
            drawOverlay({
                ctx: this.ctx,
                renderer: this,
                visibleRange
            });
        }
    }

    render() {
        this.drawBackground();
        this.drawWallMarks();
        this.drawGridLines();
        this.drawOverlays();
    }
}

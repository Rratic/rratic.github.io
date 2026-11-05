export class Angel {
    constructor(navigation) {
        this.navigation = navigation;
        this.segmentIndex = 0;
        this.moveStep = 0;
        this.visitedCellSteps = new Map();
        this.recordCurrentVisit();
    }

    setSegmentIndex(nextIndex) {
        if (!Number.isInteger(nextIndex) || nextIndex < 0) {
            throw new Error("segmentIndex must be a non-negative integer.");
        }
        this.segmentIndex = nextIndex;
        this.recordCurrentVisit();
    }

    moveBy(delta) {
        this.moveStep += 1;
        this.setSegmentIndex(this.segmentIndex + delta);
    }

    getCell() {
        return this.navigation.getRightCellAtSegmentIndex(this.segmentIndex);
    }

    recordCurrentVisit() {
        const cell = this.getCell();
        this.visitedCellSteps.set(cellKey(cell.x, cell.y), {
            x: cell.x,
            y: cell.y,
            step: this.moveStep
        });
    }

    drawVisitedSteps(ctx, renderer, visibleRange) {
        ctx.save();
        ctx.fillStyle = "#1f3db8";
        ctx.textAlign = "center";
        ctx.textBaseline = "middle";
        ctx.font = `600 ${Math.max(12, renderer.cellSize * 0.3)}px system-ui, sans-serif`;

        for (const visit of this.visitedCellSteps.values()) {
            if (visit.x < visibleRange.minX || visit.x > visibleRange.maxX || visit.y < visibleRange.minY || visit.y > visibleRange.maxY) {
                continue;
            }
            const cellOrigin = renderer.worldToScreen(visit.x, visit.y);
            const centerX = cellOrigin.x + renderer.cellSize * 0.5;
            const centerY = cellOrigin.y - renderer.cellSize * 0.5;
            ctx.fillText(String(visit.step), centerX, centerY);
        }
        ctx.restore();
    }

    draw(ctx, renderer) {
        const cell = this.getCell();
        const cellOrigin = renderer.worldToScreen(cell.x, cell.y);
        const centerX = cellOrigin.x + renderer.cellSize * 0.5;
        const centerY = cellOrigin.y - renderer.cellSize * 0.5;
        const radius = renderer.cellSize * 0.22;

        ctx.save();
        ctx.beginPath();
        ctx.fillStyle = "#ff6a00";
        ctx.arc(centerX, centerY, radius, 0, Math.PI * 2);
        ctx.fill();
        ctx.restore();
    }
}

function cellKey(x, y) {
    return `${x},${y}`;
}

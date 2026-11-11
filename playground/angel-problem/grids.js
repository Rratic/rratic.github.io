export const CellState = Object.freeze({
    /* 空格，自由格或者未被禁过的规避格 */
    SPACE: 0,
    /* 右集中的被禁格 */
    BANNED: 1,
    /* 左集中的曾经被禁格（只增不减） */
    AVOIDED: 2
});

const DEFAULT_CHUNK_SIZE = 32;

// 将 chunk 索引（有符号）偏置后装入 16 位；假定 |cellX|,|cellY| < 65536
const CHUNK_PACK_BIAS = 32768;

/*
方格坐标使用整数对 (x, y) 表示，x 向右，y 向上。

切分 chunk 的坐标计算：
- chunkX = floor(cellX / chunkSize), chunkY = floor(cellY / chunkSize).
- localX = cellX - chunkX * chunkSize, localY = cellY - chunkY * chunkSize.
- index = localY * chunkSize + localX.
*/

export class ChunkedGridMap {
    constructor(chunkSize = DEFAULT_CHUNK_SIZE) {
        this.chunkSize = chunkSize;
        this.chunkArea = chunkSize * chunkSize;
        this.chunks = new Map();
    }

    clear() {
        this.chunks.clear();
    }

    toChunkCoord(cell) {
        return Math.floor(cell / this.chunkSize);
    }

    toLocalCoord(cell, chunkCoord) {
        return cell - chunkCoord * this.chunkSize;
    }

    toChunkIndex(localX, localY) {
        return localY * this.chunkSize + localX;
    }

    chunkHash(chunkX, chunkY) {
        return (
            ((chunkX + CHUNK_PACK_BIAS) << 16) |
            ((chunkY + CHUNK_PACK_BIAS) & 0xffff)
        );
    }

    unpackChunkHash(hash) {
        return {
            chunkX: (hash >>> 16) - CHUNK_PACK_BIAS,
            chunkY: (hash & 0xffff) - CHUNK_PACK_BIAS
        };
    }

    getChunk(chunkX, chunkY, createIfMissing = false) {
        const hash = this.chunkHash(chunkX, chunkY);
        let chunk = this.chunks.get(hash);
        if (!chunk && createIfMissing) {
            chunk = new Uint8Array(this.chunkArea);
            this.chunks.set(hash, chunk);
        }
        return chunk;
    }

    getCellState(cellX, cellY) {
        const chunkX = this.toChunkCoord(cellX);
        const chunkY = this.toChunkCoord(cellY);
        const chunk = this.getChunk(chunkX, chunkY, false);
        if (!chunk) {
            return CellState.SPACE;
        }

        const localX = this.toLocalCoord(cellX, chunkX);
        const localY = this.toLocalCoord(cellY, chunkY);
        return chunk[this.toChunkIndex(localX, localY)];
    }

    setCellState(cellX, cellY, nextState) {
        const chunkX = this.toChunkCoord(cellX);
        const chunkY = this.toChunkCoord(cellY);
        const shouldCreateChunk = nextState !== CellState.SPACE;
        const chunk = this.getChunk(chunkX, chunkY, shouldCreateChunk);
        if (!chunk) {
            return;
        }

        const localX = this.toLocalCoord(cellX, chunkX);
        const localY = this.toLocalCoord(cellY, chunkY);
        chunk[this.toChunkIndex(localX, localY)] = nextState;
    }

    /** 魔鬼禁用：右集被禁格 */
    banCell(cellX, cellY) {
        this.setCellState(cellX, cellY, CellState.BANNED);
    }

    /**
     * 路径更新后，将已进入 V_κ 的被禁格升级为规避格（BANNED → AVOIDED，单调不降）。
     */
    absorbBannedIntoLeftSet(leftSet) {
        for (const key of leftSet) {
            const comma = key.indexOf(",");
            const x = Number(key.slice(0, comma));
            const y = Number(key.slice(comma + 1));
            if (this.getCellState(x, y) === CellState.BANNED) {
                this.setCellState(x, y, CellState.AVOIDED);
            }
        }
    }

    forEachCellWithStateInRange(range, state, visitCell) {
        const minChunkX = this.toChunkCoord(range.minX);
        const maxChunkX = this.toChunkCoord(range.maxX);
        const minChunkY = this.toChunkCoord(range.minY);
        const maxChunkY = this.toChunkCoord(range.maxY);

        for (let chunkY = minChunkY; chunkY <= maxChunkY; chunkY += 1) {
            for (let chunkX = minChunkX; chunkX <= maxChunkX; chunkX += 1) {
                const chunk = this.getChunk(chunkX, chunkY, false);
                if (!chunk) {
                    continue;
                }

                for (let localY = 0; localY < this.chunkSize; localY += 1) {
                    for (let localX = 0; localX < this.chunkSize; localX += 1) {
                        const index = this.toChunkIndex(localX, localY);
                        if (chunk[index] !== state) {
                            continue;
                        }

                        const cellX = chunkX * this.chunkSize + localX;
                        const cellY = chunkY * this.chunkSize + localY;
                        if (cellX < range.minX || cellX > range.maxX || cellY < range.minY || cellY > range.maxY) {
                            continue;
                        }

                        visitCell(cellX, cellY);
                    }
                }
            }
        }
    }

    /** 遍历所有非 SPACE 格子；visitCell(x, y, state)。 */
    forEachOccupiedCell(visitCell) {
        for (const [hash, chunk] of this.chunks) {
            const { chunkX, chunkY } = this.unpackChunkHash(hash);

            for (let localY = 0; localY < this.chunkSize; localY += 1) {
                for (let localX = 0; localX < this.chunkSize; localX += 1) {
                    const index = this.toChunkIndex(localX, localY);
                    const state = chunk[index];
                    if (state === CellState.SPACE) {
                        continue;
                    }
                    visitCell(
                        chunkX * this.chunkSize + localX,
                        chunkY * this.chunkSize + localY,
                        state
                    );
                }
            }
        }
    }
}

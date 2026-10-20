export class Board {
	length
	height
	contents
	/* 
	 * [cell.id]
	 * piece type id
	 * 
	 * [cell.ownership]
	 * owner's id, may be undefined
	 * 
	 * [cell.flag]
	 * times the piece has moved
	 */
	count
	/* 
	 * count[ownership][id] = number
	 */
	config
	/* 
	 * [ctrans]
	 * translate table, piece type name => type id
	 * must register "void"
	 * 
	 * [cells]
	 * data of piece types
	 * * [cells[id].type]
	 * * "v" for void, "p" for piece
	 * * [cells[id].moves]
	 * * moving handler
	 * 
	 * [rival]
	 * function to check whether two sides are enemies
	 * 
	 * [move]
	 * basic moving function
	 */
	getIndex(x, y) {
		return this.contents[x * this.length + y];
	}
	setIndex(x, y, cell) {
		let ind = x * this.length + y;
		let ocell = this.contents[ind];
		if (ocell.ownership != undefined) {
			this.count[ocell.ownership][ocell.id] -= 1;
		}
		if (cell.ownership != undefined) {
			this.count[cell.ownership][cell.id] += 1;
		}
		this.contents[ind] = cell;
	}
	setIndexKey(x, y, key, value) {
		this.contents[x * this.length + y][key] = value;
	}
	moveTo(fx, fy, tx, ty) {
		let cell = this.getIndex(fx, fy);
		if (cell["flag"] == undefined)
			cell["flag"] = 1;
		else
			cell["flag"] += 1;
		this.setIndex(fx, fy, { id: this.config.ctrans["void"] });
		this.setIndex(tx, ty, cell);
	}
	tryMoveBy(x, y, dx, dy) {
		return this.config.move(x, y, dx, dy);
	}
	isIn(x, y) {
		return 0 <= x && x < this.length &&
			0 <= y && y < this.height;
	}
	cellType(x, y) {
		return this.config.cells[this.getIndex(x, y).id].type;
	}
	cellRival(x, y, me) {
		return this.config.rival(this.getIndex(x, y).ownership, me);
	}
	isVoid(x, y) {
		return this.cellType(x, y) == "v";
	}
	isPiece(x, y) {
		return this.cellType(x, y) == "p";
	}
	isAlly(x, y, me) {
		return this.isPiece(x, y) && !this.cellRival(x, y, me);
	}
	isEnemy(x, y, me) {
		return this.isPiece(x, y) && this.cellRival(x, y, me);
	}
	existCount() {
		let cnt = 0;
		for (let owned of this.count)
			for (let num of owned)
				cnt += num;
		return cnt;
	}
	existAllies(ownerId) {
		let listx = [], listy = [];
		for (let x = 0; x < this.height; x++) {
			for (let y = 0; y < this.length; y++) {
				if (this.isAlly(x, y, ownerId)) {
					listx.push(x);
					listy.push(y);
				}
			}
		}
		return [listx, listy];
	}
	copy() {
		let board = new Board();
		board.length = this.length;
		board.height = this.height;
		board.contents = structuredClone(this.contents);
		board.count = structuredClone(this.count);
		board.config = this.config;
		return board;
	}
}

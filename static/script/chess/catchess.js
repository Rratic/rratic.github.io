import { Board } from "./chess_base.js";

function cat_svg(color, color2, direction) {
	let svg = new Image();
	let circle_ml =
		direction == 1 ?
			`<circle cx="300" cy="260" r="10" fill="${color2}"/><circle cx="370" cy="260" r="10" fill="${color2}"/>` :
			`<circle cx="180" cy="220" r="10" fill="${color2}"/><circle cx="110" cy="220" r="10" fill="${color2}"/>`;
	let ml = `<svg xmlns="http://www.w3.org/2000/svg" width="480" height="480">
	<path fill="${color}" d="M 418.606,150 A 200 200 0 0 ${direction} 61.394,330 M 61.394,330 L 110,270 L 180,270 L 240,330 L 240,150 L 300,210 L 370,210 L 418.606,150 Z"/>${circle_ml}</svg>`;
	svg.src = "data:image/svg+xml;base64," + window.btoa(ml);
	return svg;
}

const cat_svg_cache = [
	[null, cat_svg("white", "black", 0), cat_svg("black", "orange", 0), cat_svg("orange", "white", 0)],
	[null, cat_svg("white", "black", 1), cat_svg("black", "orange", 1), cat_svg("orange", "white", 1)]
]

function move_white_cat(b, x, y, nx, ny, n2x, n2y) {
	let cell = b.getIndex(nx, ny);
	cell.id = cell.id == 3 ? 1 : cell.id + 1;
	b.moveTo(x, y, n2x, n2y);
}

function moves_white_cat(b, x, y) {
	let list = [];
	for (let rx = -1; rx <= 1; rx++) {
		for (let ry = -1; ry <= 1; ry++) {
			if (rx == 0 && ry == 0) continue;
			let np = b.tryMoveBy(x, y, rx, ry);
			if (np != null && b.isPiece(np[0], np[1])) {
				let np2 = b.tryMoveBy(x, y, rx * 2, ry * 2);
				if (np2 != null && b.isVoid(np2[0], np2[1])) {
					list.push({
						tx: np2[0],
						ty: np2[1],
						on: [x, y, np[0], np[1], np2[0], np2[1]]
					});
				}
			}
		}
	}
	return list;
}

function _ch_void_black_cat(b, list, x, y, dx, dy) {
	let np = b.tryMoveBy(x, y, dx, dy);
	if (np != null && b.isVoid(np[0], np[1])) {
		list.push({ tx: np[0], ty: np[1] });
		return true;
	}
	return false;
}
function _ch_enemy_black_cat(b, list, me, x, y, dx, dy) {
	let np = b.tryMoveBy(x, y, dx, dy);
	if (np != null && b.isEnemy(np[0], np[1], me)) {
		list.push({ tx: np[0], ty: np[1] });
	}
}
function moves_black_cat(b, x, y) {
	let list = [];
	let cell = b.getIndex(x, y);
	let me = cell.ownership;
	let front = b.config.front(me);
	let flag = _ch_void_black_cat(b, list, x, y, 0, front); // front
	_ch_enemy_black_cat(b, list, me, x, y, 1, front); // front-left
	_ch_enemy_black_cat(b, list, me, x, y, -1, front); // front-right
	if (cell.flag == undefined && flag) {
		_ch_void_black_cat(b, list, x, y, 0, front * 2);
	}
	return list;
}

function moves_orange_cat(b, x, y) {
	let list = [];
	let me = b.getIndex(x, y).ownership;
	for (let i = 0; i < 4; i++) {
		let rx = [0, 0, 1, -1][i];
		let ry = [1, -1, 0, 0][i];
		let np = b.tryMoveBy(x, y, rx, ry);
		if (np == null || b.isAlly(np[0], np[1], me))
			continue;
		list.push({ tx: np[0], ty: np[1] });
	}
	return list;
}

export let catchessRules = {
	__init__: function (size) {
		let board = new Board();
		board.length = size;
		board.height = size;
		board.contents = new Array(size * size);
		board.count = [[0, size, size, 0], [0, size, size, 0]];
		board.config = {
			"ctrans": { "void": 0, "white": 1, "black": 2, "orange": 3 },
			"cells": [
				{ type: "v", },
				{ type: "p", moves: moves_white_cat },
				{ type: "p", moves: moves_black_cat },
				{ type: "p", moves: moves_orange_cat },
			],
			"rival": (a, b) => (a != b),
			"front": (ownership) => (ownership == 0 ? 1 : -1),
			"first_player_id": 1,
			"turn_player_id": (o) => (1 - o),
			"move": function (x, y, dx, dy) {
				let x0 = x + dx;
				let y0 = y + dy;
				if (x0 < 0 || x0 >= size) return null;
				if (y0 >= size) y0 -= size;
				if (y0 < 0) y0 += size;
				return [x0, y0];
			},
			"pieces_initialize": function () {
				let contents = board.contents;
				for (let i = 0; i < size; i++) {
					let lines = i * size;
					for (let j = 1; j <= 2; j++) {
						contents[lines + j - 1] = { id: j, ownership: 0 };
						contents[lines + size - j] = { id: j, ownership: 1 };
					}
					for (let j = 2; j < size - 2; j++) {
						contents[lines + j] = { id: 0 };
					}
				}
			},
			"canvas_display": function (ctx, wi, o) {
				let board_size = wi * size;
				ctx.clearRect(0, 0, board_size, board_size + 32);
				ctx.fillStyle = "#C19B6F";
				ctx.fillRect(0, 0, board_size, board_size + 32);
				for (let i = 0; i < size; i++) {
					for (let j = 0; j < size; j++) {
						let obj = board.contents[i * size + j];
						if (obj.id == 0) continue;
						let image = cat_svg_cache[obj.ownership][obj.id];
						ctx.drawImage(image, i * wi, j * wi + 16, wi, wi);
					}
				}
				ctx.fillStyle = "#F0FCFF";
				ctx.fillRect(0, o == 0 ? board_size + 16 : 0, board_size, 16);
				ctx.fillStyle = "#7BCFA6";
				ctx.fillRect(0, o == 0 ? 0 : board_size + 16, board_size, 16);
			},
			"draw_chosen": function (ctx, x, y, wi) {
				ctx.fillStyle = "#FFFF3f4F";
				ctx.fillRect(x * wi + 4, y * wi + 16 + 4, wi - 8, wi - 8);
			},
			"draw_target": function (ctx, x, y, wi) {
				ctx.fillStyle = "#3f3fFF20";
				ctx.fillRect(x * wi + 4, y * wi + 16 + 4, wi - 8, wi - 8);
			},
		}
		return board;
	},
	__winned(board, player) {
		let enm = board.existAllies(1 - player);
		let cnt = enm[0].length;
		if (cnt == 0) return true;
		for (let ind = 0; ind < cnt; ind++) {
			let x = enm[0][ind];
			let y = enm[1][ind];
			let cell = board.getIndex(x, y);
			let moves = board.config.cells[cell.id].moves(board, x, y);
			if (moves.length != 0) return false;
		}
		return true;
	},
	__evaluate(board, player) {
		let t = [0, 0];
		for (let j = 0; j <= 1; j++) {
			let c = board.count[j];
			let d = 0;
			if (c[2] + c[3] == 0) d = c[1];
			else {
				d = c[1] <= 5 ? 5 : c[1];
				d += 2 * c[2] + 3 * c[3];
			}
			t[j] = d;
		}
		return t[player] - t[1 - player];
	},
	__play_without_ai(board, x, y, tx, ty, on) {
		if (on == undefined) {
			board.moveTo(x, y, tx, ty);
		}
		else {
			move_white_cat(board, ...on);
		}
	},
	/* 
	 * @ref OI Wiki: Alpha-Beta 剪枝
	 * 计算赢面
	 * α: 最大下界
	 * β: 最小上界
	 * 
	 */
	alphabeta(board, depth, α, β, player, tellMove) {
		// terminalize
		let allies = board.existAllies(player);
		let cnt_ally = allies[0].length;
		let count = board.existCount();
		if (depth == 0 || cnt_ally == 0 || cnt_ally == count) {
			return this.__evaluate(board, player);
		}
		// branches
		let typeBigger = (player == 0); // is 极大节点
		let flag = false, flagAv = true;
		let saveLast = undefined;
		let bridge = undefined;
		for (let ind = 0; ind < cnt_ally; ind++) {
			let x = allies[0][ind];
			let y = allies[1][ind];
			let cell = board.getIndex(x, y);
			let moves = board.config.cells[cell.id].moves(board, x, y);
			for (let move of moves) {
				flagAv = false;
				let child = board.copy();
				let tx = move.tx;
				let ty = move.ty;
				let on = move.on;
				this.__play_without_ai(child, x, y, tx, ty, on);
				let value = this.alphabeta(child, depth - 1, α, β, 1 - player, false);
				if (tellMove) saveLast = [value, x, y, tx, ty, on];
				if (typeBigger) {
					if (value > α) {
						α = value;
						bridge = saveLast;
					}
				}
				else {
					if (value < β) {
						β = value;
						bridge = saveLast;
					}
				}
				if (β <= α) { // cut-off
					flag = true;
					break;
				}
			}
			if (flag) {
				break;
			}
		}
		if (flagAv) return this.__evaluate(board, player); // missed terminal
		let result = typeBigger ? α : β;
		if (!tellMove) {
			return result;
		}
		else {
			if (bridge == undefined) bridge = saveLast;
			bridge[0] = result;
			return bridge;
		}
	},
	__play_ai(board, ownsId) {
		let value = this.alphabeta(board, 4, -Infinity, Infinity, ownsId, true);
		if (typeof (value) == "object") {
			if (value.length == 1) {
				console.error("unexpected: no proper behaviour");
			}
			this.__play_without_ai(board, value[1], value[2], value[3], value[4], value[5]);
		}
	}
}

import { catchessRules } from "./catchess.js";

let board = null;

function initialize_chess(canvas, mode) {
	let context = canvas.getContext("2d");
	let wi = Math.floor(canvas.width / board.length);
	let status = -1; // pending
	let chosen_x = undefined;
	let chosen_y = undefined;
	let moves_cache = null;

	canvas.addEventListener("click", function (event) {
		if (status == -1) return;
		let x = Math.floor(event.offsetX / wi);
		let y = Math.floor((event.offsetY - 16) / wi);
		if (!board.isIn(x, y)) return;
		if (moves_cache != null && !board.isAlly(x, y, status)) {
			let flag = true;
			for (let obj of moves_cache) {
				if (obj.tx == x && obj.ty == y) {
					flag = obj.on;
				}
			}
			if (flag !== true) {
				moves_cache = null;
				catchessRules.__play_without_ai(board, chosen_x, chosen_y, x, y, flag);
				status = board.config.turn_player_id(status);
				board.config.canvas_display(context, wi, status);
				if (mode != 0 && status == 0) { // AI must be player 0
					setTimeout(() => {
						catchessRules.__play_ai(board, mode);
						status = 1;
						board.config.canvas_display(context, wi, 1);
					}, 500);
					status = -1;
				}
			}
		}
		else if (board.isAlly(x, y, status)) {
			board.config.canvas_display(context, wi, status);
			board.config.draw_chosen(context, x, y, wi);
			let cell = board.getIndex(x, y);
			let moves = board.config.cells[cell.id].moves(board, x, y);
			if (moves == []) return;
			chosen_x = x;
			chosen_y = y;
			for (let obj of moves) {
				board.config.draw_target(context, obj.tx, obj.ty, wi);
			}
			moves_cache = moves;
		}
	});
	board.config.pieces_initialize();
	status = board.config.first_player_id;
	board.config.canvas_display(context, wi, status);
}

function init(maxWidth, size, mode) {
	let cvs = document.createElement("canvas");
	cvs.width = maxWidth;
	cvs.height = maxWidth + 32;
	board = catchessRules.__init__(size);
	initialize_chess(cvs, mode);
	return cvs;
}

function launch() {
	let container = document.getElementById("canvas_box");
	let size = Number(document.getElementById("input-board-size").value);
	if (size == 0) size = 8;
	let mode = Number(document.getElementById("select-mode").value);
	container.replaceChildren(init(64 * size, size, mode));
}

window.launch = launch;

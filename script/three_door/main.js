class Simulation {
	secrets
	knowledge
}

class SimulationConfig {
	repetition
	random_generator
	simulator_init
	simulator_open
}

function random_js() {
	return Math.floor(Math.random() * 3);
}

function init_plain(config, sim) {
	sim.secrets = [false, false, false];
	sim.secrets[config.random_generator()] = true;
}

function init_with_knowledge(config, sim) {
	let prize = config.random_generator();
	sim.secrets = [false, false, false];
	sim.secrets[prize] = true;
	sim.knowledge = prize;
}

function open_with_knowledge(sim, chosen) {
	if (sim.knowledge == chosen) {
		return chosen == 0 ? 1 : 0;
	}
	else {
		return 3 - chosen - sim.knowledge;
	}
}

function open_random_enumerate(sim, chosen) {
	while (true) {
		let it = random_js();
		if (it != chosen && !sim.secrets[it]) {
			return it;
		}
	}
}

var interval = -1;
function launch() {
	let config = new SimulationConfig();
	let r = Number(document.getElementById("select-repetition").value);
	let s = document.getElementById("select-scheme").value == "a";
	config.repetition = r;
	config.random_generator = random_js;
	config.simulator_init = s ? init_with_knowledge : init_plain;
	config.simulator_open = s ? open_with_knowledge : open_random_enumerate;
	if (interval != -1) {
		clearInterval(interval);
		interval = -1;
	}
	if (r != 0) {
		document.getElementById("choose-door").style.display = "none";
		document.getElementById("choose").style.display = "none";
		run(config);
	}
	else {
		run_nonauto(config);
	}
}

function output(st) {
	document.getElementById("output").innerText =
		`换门：${st[0][0]}/${st[0][1]}；
		不换门：${st[1][0]}/${st[1][1]}`;
}

function run(config) {
	let r = config.repetition
	let cnt = 0;
	let statistics = [[0, r], [0, r]];
	while (cnt < r * 2) {
		let sim = new Simulation();
		config.simulator_init(config, sim);
		let c1 = -1;
		c1 = config.random_generator();
		let c2 = config.simulator_open(sim, c1);
		if (cnt < r) {
			if (sim.secrets[3 - c1 - c2]) {
				statistics[0][0] += 1;
			}
		}
		else {
			if (sim.secrets[c1]) {
				statistics[1][0] += 1;
			}
		}
		cnt += 1;
	}
	output(statistics);
}

var chosen = -1;
var change = -1;

function run_nonauto(config) {
	let statistics = [[0, 0], [0, 0]];
	let status = 0;
	let sim = null;
	let c1 = 0;
	let c2 = 0;
	let c = true;
	interval = setInterval(function () {
		if (status == 0) {
			sim = new Simulation();
			config.simulator_init(config, sim);
			document.getElementById("choose-door").style.display = "";
			status = 1;
		}
		else if (status == 1 && chosen != -1) {
			c1 = chosen;
			c2 = config.simulator_open(sim, c1);
			window.alert("对方打开了门" + "ABC".at(c2));
			document.getElementById("choose-door").style.display = "none";
			document.getElementById("choose").style.display = "";
			chosen = -1;
			status = 2;
		}
		else if (status == 2 && change != -1) {
			c = change == 0;
			document.getElementById("choose").style.display = "none";
			change = -1;
			status = 3;
		}
		else if (status == 3) {
			if (c) {
				if (sim.secrets[3 - c1 - c2]) {
					statistics[0][0] += 1;
				}
				statistics[0][1] += 1;
			}
			else {
				if (sim.secrets[c1]) {
					statistics[1][0] += 1;
				}
				statistics[1][1] += 1;
			}
			output(statistics);
			status = 0;
		}
	}, 10);
}

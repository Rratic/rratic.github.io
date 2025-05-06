class Simulation {
	secrets
	knowledge
}

class SimulationConfig {
	repetition
	random_source
	random_algorithm
	simulator_init
	simulator_open
}

function random_js() {
	return Math.floor(Math.random() * 3)
}

function init_plain(sim) {
	sim.secrets = [false, false, false]
	sim.secrets[sim.random_source()] = true
}
function init_with_knowledge(sim) { }

function open_with_knowledge() { }

function open_enumerate() { }

function open_random_enumerate() { }

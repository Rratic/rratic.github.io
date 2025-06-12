// initialization
const scene = new THREE.Scene();

const camera = new THREE.PerspectiveCamera(
	75, // 视场角
	window.innerWidth / window.innerHeight, // 宽高比
	0.1, // 近裁剪面
	1000 // 远裁剪面
);
camera.position.z = 5;

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.setClearColor(0xffffff);
document.body.appendChild(renderer.domElement);

const controls = new THREE.OrbitControls(camera, renderer.domElement);
controls.enableDamping = true;
controls.dampingFactor = 0.05;
// controls.autoRotate = true;
// controls.autoRotateSpeed = 1;

var debug = false;
var autoRotate = true;

const toggleDebug = document.getElementById('toggle-debug');
toggleDebug.addEventListener('change', (e) => {
	debug = e.target.checked;
});

const toggleAutoRotate = document.getElementById('toggle-auto-rotate');
toggleAutoRotate.addEventListener('change', (e) => {
	autoRotate = e.target.checked;
});

window.addEventListener('resize', () => {
	camera.aspect = window.innerWidth / window.innerHeight;
	camera.updateProjectionMatrix();
	renderer.setSize(window.innerWidth, window.innerHeight);
});

// definitions
const edgeMaterial = new THREE.LineBasicMaterial({
	color: 0x000000,
	linewidth: 2,
});

function createTextTexture(text) {
	const canvas = document.createElement('canvas');
	canvas.width = 64;
	canvas.height = 64;
	const ctx = canvas.getContext('2d');

	ctx.fillStyle = 'black';
	ctx.font = 'Bold 40px Arial';
	ctx.textAlign = 'center';
	ctx.textBaseline = 'middle';
	ctx.fillText(text, 32, 32);

	return canvas.toDataURL();
}

function build_polyhydron(vertices, faces, color) {
	const geometry = new THREE.BufferGeometry();

	geometry.setFromPoints(vertices.map(v => new THREE.Vector3(...v)));

	// generate faces
	const triangulatedFaces = [];
	faces.forEach(face => {
		if (face.length === 3) {
			triangulatedFaces.push(...face);
		} else if (face.length > 3) {
			for (let i = 1; i < face.length - 1; i++) {
				triangulatedFaces.push(face[0], face[i], face[i + 1]);
			}
		}
	});
	geometry.setIndex(triangulatedFaces);

	const faceMaterial = new THREE.MeshBasicMaterial({
		color: color,
		wireframe: false,
		transparent: true,
		opacity: 0.3,
		side: THREE.DoubleSide
	});
	const mesh = new THREE.Mesh(geometry, faceMaterial);

	// generate edges
	const edgeVertices = [];
	const edgeGeometry = new THREE.BufferGeometry();
	faces.forEach(face => {
		let l = face.length;
		for (let i = 0; i < l; i++) {
			let next = i + 1;
			if (next == l) next = 0;
			edgeVertices.push(...vertices[face[i]], ...vertices[face[next]]);
		}
	});
	edgeGeometry.setAttribute(
		'position',
		new THREE.Float32BufferAttribute(edgeVertices, 3)
	);

	const edgesMesh = new THREE.LineSegments(edgeGeometry, edgeMaterial);

	// group
	const group = new THREE.Group();
	group.add(mesh);
	group.add(edgesMesh);

	if (debug) {
		const loader = new THREE.TextureLoader();
		const textMaterial = new THREE.SpriteMaterial({
			map: loader.load(createTextTexture("0")), // 初始纹理
			transparent: true
		});

		vertices.forEach((vertex, i) => {
			const sprite = new THREE.Sprite(textMaterial.clone());
			sprite.material.map = loader.load(createTextTexture(i.toString()));
			sprite.position.set(...vertex);
			sprite.scale.set(0.3, 0.3, 1); // 文本大小
			sprite.position.multiplyScalar(1.1); // 稍微外移避免重叠

			group.add(sprite);
		});
	}

	return group;
}

// polyhedra data
const files = [{
	title: "正六面体",
	value: "cube",
}, {
	title: "正四面体",
	value: "tetrahedron",
}, {
	title: "正十二面体",
	value: "dodecahedron",
}];

function load_file(path) {
	let promise = fetch(path)
		.then(response => {
			if (!response.ok) {
				console.error(response);
				return null;
			}
			return response.json();
		});
	return promise;
}

const shapeSelector = document.getElementById('select-shape');
files.forEach(file => {
	const option = document.createElement('option');
	option.value = file.value;
	option.textContent = file.title;
	shapeSelector.appendChild(option);
});

const loaded = [];
document.getElementById('button-add').addEventListener('click', () => {
	const value = shapeSelector.value;
	const selectedColor = document.getElementById('select-color').value;
	let promise = load_file("/assets/polyhedra/" + value + ".json");
	promise.then(data => {
		let group = build_polyhydron(data["vertices"], data["faces"], selectedColor);
		scene.add(group);
		loaded.push(group);
	});
});

// run
function animate() {
	requestAnimationFrame(animate);
	if (autoRotate) {
		for (let group of loaded) {
			group.rotation.x += 0.01;
		}
	}
	controls.update();
	renderer.render(scene, camera);
}
animate();

// initialization
var useOrthographicCamera = false;
var debug = false;
var autoRotate = false;

const scene = new THREE.Scene();

const renderer = new THREE.WebGLRenderer({ alpha: true, antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
renderer.setClearColor(0xffffff);
document.body.appendChild(renderer.domElement);

var camera;
var controls;

function buildCamera() {
	const aspect = window.innerWidth / window.innerHeight;
	if (useOrthographicCamera) {
		const viewSize = 10;
		camera = new THREE.OrthographicCamera(
			-viewSize * aspect / 2,
			viewSize * aspect / 2,
			viewSize / 2,
			-viewSize / 2,
			0.1, // 近裁剪面
			1000 // 远裁剪面
		);
	}
	else {
		camera = new THREE.PerspectiveCamera(
			75, // 视场角
			aspect,
			0.1,
			1000
		);
	}

	camera.position.z = 5;

	controls = new THREE.OrbitControls(camera, renderer.domElement);
	controls.enableDamping = true;
	controls.dampingFactor = 0.05;
}

// toggles
const toggleOrthographicCamera = document.getElementById('toggle-orthographic-camera');
toggleOrthographicCamera.addEventListener('change', (e) => {
	useOrthographicCamera = e.target.checked;
	buildCamera();
});

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
const standardizedColors = [
	0xffffff, // 0
	0xffffff, // 1
	0xffffff, // 2
	0xffc000,
	0xff8000,
	0xff0a00,
	0x30a0ff,
	0xffffff, // 7
	0x0ab0e0,
	0xffffff, // 9
	0x0ab08a,
];

function createFaceMaterial(config) {
	const color = config["face-color"];
	const material = new THREE.MeshBasicMaterial({
		color: color,
		wireframe: false,
		side: THREE.DoubleSide
	});
	if (config["face-mode"] == "transparent") {
		material.transparent = true;
		material.opacity = 0.3;
	}
	return material;
}

function createEdgeMaterial(config) {
	return new THREE.LineBasicMaterial({
		color: config["edge-color"],
		linewidth: 2,
	});
}

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

function tag_1_by_id(i) {
	return String(i);
}

function tag_2_by_id(i) {
	return String.fromCharCode(64 + i); // 'A': 65
}

function tag_3_by_id(i) {
	return String.fromCharCode(96 + i); // 'a': 97
}

function buildPolyhydronGroup(vertices, faces, config) {
	const group = new THREE.Group();

	const geometry = new THREE.BufferGeometry();
	geometry.setFromPoints(vertices.map(v => new THREE.Vector3(...v)));

	// generate faces
	if (config["face-mode"] != "none") {
		if (config["face-mode"] == "standardized") {
			faces.forEach(face => {
				const faceGeometry = geometry.clone();
				const triangulatedFaces = [];
				for (let i = 1; i < face.length - 1; i++) {
					triangulatedFaces.push(face[0], face[i], face[i + 1]);
				}
				faceGeometry.setIndex(triangulatedFaces);

				config["face-color"] = standardizedColors[face.length];
				const faceMaterial = createFaceMaterial(config);
				const mesh = new THREE.Mesh(faceGeometry, faceMaterial);
				group.add(mesh);
			});
		}
		else {
			const triangulatedFaces = [];
			faces.forEach(face => {
				for (let i = 1; i < face.length - 1; i++) {
					triangulatedFaces.push(face[0], face[i], face[i + 1]);
				}
			});
			geometry.setIndex(triangulatedFaces);

			const faceMaterial = createFaceMaterial(config);
			const mesh = new THREE.Mesh(geometry, faceMaterial);
			group.add(mesh);
		}
	}

	// generate edges
	if (config["edge-mode"] != "none") {
		const set = new Set();
		const edgeVertices = [];
		const edgeGeometry = new THREE.BufferGeometry();
		faces.forEach(face => {
			let l = face.length;
			for (let i = 0; i < l; i++) {
				let next = i + 1;
				if (next == l) next = 0;
				let v1 = face[i]; let v2 = face[next];

				// min << 16 | max; 2^32 < MAX_SAFE_INTEGER
				let key = (v1 < v2) ? v1 << 16 | v2 : v2 << 16 | v1;
				if (!set.has(key)) {
					set.add(key);
					edgeVertices.push(...vertices[v1], ...vertices[v2]);
				}
			}
		});
		edgeGeometry.setAttribute(
			'position',
			new THREE.Float32BufferAttribute(edgeVertices, 3)
		);
		const edgeMaterial = createEdgeMaterial(config);

		const edgesMesh = new THREE.LineSegments(edgeGeometry, edgeMaterial);
		group.add(edgesMesh);
	}

	// generate debug text
	if (debug) {
		const loader = new THREE.TextureLoader();
		const textMaterial = new THREE.SpriteMaterial({
			map: loader.load(createTextTexture("0")), // 初始纹理
			transparent: true
		});

		// 顶点 tag
		vertices.forEach((vertex, i) => {
			const sprite = new THREE.Sprite(textMaterial.clone());
			sprite.material.map = loader.load(createTextTexture(tag_1_by_id(i)));
			sprite.position.set(...vertex);
			sprite.scale.set(0.3, 0.3, 1); // 文本大小
			sprite.position.multiplyScalar(1.1); // 稍微外移避免重叠

			group.add(sprite);
		});

		// 棱 tag
		// todo

		// 面 tag
		let fi = 1;
		faces.forEach(face => {
			let l = face.length;
			let x = 0.0; let y = 0.0; let z = 0.0;
			for (let i = 0; i < l; i++) {
				x += vertices[face[i]][0];
				y += vertices[face[i]][1];
				z += vertices[face[i]][2];
			}

			const sprite = new THREE.Sprite(textMaterial.clone());
			sprite.material.map = loader.load(createTextTexture(tag_3_by_id(fi)));
			sprite.position.set(x / l, y / l, z / l);
			sprite.scale.set(0.3, 0.3, 1); // 文本大小
			sprite.position.multiplyScalar(1.1); // 稍微外移避免重叠

			group.add(sprite);
			fi += 1;
		});
	}

	return group;
}

// polyhedra data
const files = [{
	title: "正四面体",
	value: "tetrahedron",
}, {
	title: "正六面体",
	value: "cube",
}, {
	title: "正八面体",
	value: "octahedron"
}, {
	title: "正十二面体",
	value: "dodecahedron",
}, {
	title: "正二十面体",
	value: "icosahedron",
}/*, {
	title: "小星形十二面体",
	value: "small_stellated_dodecahedron"
}, {
	title: "大十二面体",
	value: "great_dodecahedron"
}, {
	title: "大星形十二面体",
	value: "great_stellated_dodecahedron"
}, {
	title: "大二十面体",
	value: "great_icosahedron"
}*/, {
	title: "截角四面体",
	value: "truncated_tetrahedron"
}, {
	title: "立方八面体",
	value: "cuboctahedron",
}, {
	title: "截角八面体",
	value: "truncated_octahedron",
}, {
	title: "截角立方体",
	value: "truncated_cube",
}, {
	title: "小斜方截半立方体",
	value: "rhombicuboctahedron",
}, {
	title: "扭棱立方体",
	value: "snub_cube",
}, {
	title: "大斜方截半立方体",
	value: "great_rhombicuboctahedron",
}, {
	title: "截半二十面体",
	value: "icosidodecahedron",
}, {
	title: "截角二十面体",
	value: "truncated_icosahedron",
}, {
	title: "小斜方截半二十面体",
	value: "rhombicosidodecahedron",
}, {
	title: "截角十二面体",
	value: "truncated_dodecahedron",
}, {
	title: "扭棱二十面体",
	value: "snub_dodecahedron",
}, {
	title: "大斜方截半二十面体",
	value: "great_rhombicosidodecahedron",
}];

function loadJson(path) {
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

const cache = {};
const addedGroups = [];
document.getElementById('button-add').addEventListener('click', () => {
	const value = shapeSelector.value;

	const faceMode = document.getElementById('select-face-mode').value;
	const faceColor = document.getElementById('select-face-color').value;
	const edgeMode = document.getElementById('select-edge-mode').value;
	const edgeColor = document.getElementById('select-edge-color').value;

	const config = {
		"face-mode": faceMode,
		"face-color": faceColor,
		"edge-mode": edgeMode,
		"edge-color": edgeColor,
	};

	const add = data => {
		let group = buildPolyhydronGroup(data["vertices"], data["faces"], config);
		scene.add(group);
		addedGroups.push(group);
	};

	if (cache[value] == undefined) {
		let promise = loadJson("/assets/polyhedra/" + value + ".json");
		promise.then(data => {
			add(data);
			cache[value] = data;
		});
	}
	else {
		add(cache[value]);
	}
});

document.getElementById('button-clear').addEventListener('click', () => {
	scene.clear();
	addedGroups.splice(0, addedGroups.length);
});

// export
document.getElementById('exportSvgBtn').addEventListener('click', () => {
	const width = renderer.domElement.width;
	const height = renderer.domElement.height;

	var rendererSVG = new THREE.SVGRenderer();

	rendererSVG.setSize(width, height);
	rendererSVG.render(scene, camera);

	var XMLS = new XMLSerializer();
	const svgContent = XMLS.serializeToString(rendererSVG.domElement);

	// trigger download
	const blob = new Blob([svgContent], { type: 'image/svg+xml' });
	const url = URL.createObjectURL(blob);
	const a = document.createElement('a');
	a.href = url;
	a.download = 'scene.svg';
	a.click();
	URL.revokeObjectURL(url);
});

// run
function animate() {
	requestAnimationFrame(animate);
	if (autoRotate) {
		for (let group of addedGroups) {
			group.rotation.x += 0.01;
		}
	}
	controls.update();
	renderer.render(scene, camera);
}

buildCamera();
animate();

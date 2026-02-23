+++
title = "【着色器】综合的光照场景"
description = "一个综合了一、二、三内容的例子。"
date = 2026-01-18

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "图形学"]
+++

参考了[数学公式模拟大自然：雪山大海的日出日落](https://zhuanlan.zhihu.com/p/30586157351)。

---

我们回顾在[着色器（二）](@/posts/shader_2.md)中提到可以将光照方向与法方向的余弦值作为光强。这是在假定了绝对漫反射下的情形。

实际材质可能出现高光（夹角很小时近似镜面反射）。样例如下：
```glsl
vec3 lightDirection = vec3(1.0, 1.0, 1.0);

float getBallValue(float x, float y) {
    float z = sqrt(1. - x * x - y * y);
    float val = max(0., dot(lightDirection, vec3(x, y, z)) / sqrt(3.));
    return min(1., val + pow(val, 100.0));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
	float dist = length(uv);

	float value = getBallValue(uv.x, uv.y);

	fragColor = vec4(value, value, value, 1.0);
}
```

在使用 HSV 的情形下就是把这个值赋予亮度，而使用贴图就是将光强与贴图相乘。

---

我们回顾在[着色器（一）](@/posts/shader_1.md)中介绍了 [Raymarching](@/posts/shader_1.md#Raymarching). 我们假定一个简单版本：地面是从 XZ 平面上凸起的，可以询问得到 y 坐标的高度。为了获得法向量的信息，可以通过周围很小距离的高度来推测。

完整例子如下：

```glsl
vec3 lightDirection = vec3(1.0, 1.0, 1.0) / sqrt(3.);

float height(float x, float z) {
    return sin(x * 0.001) * 500. + cos(z * 0.001) * 500. - 200.;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
	vec2 uv = (fragCoord * 2. - iResolution.xy) / iResolution.y;

	vec3 ro = vec3(0, 800, -sqrt(3.)); // ray origin
	vec3 rd = normalize(vec3(uv, sqrt(3.))); // ray direction
	vec3 col = vec3(0., 0., 1.);

	float t = 0.; // distance travelled
    vec3 p = ro; // position

	for (int i = 0; i < 1000; i++) {
		p = ro + rd * t;
        if (p.y < 0.) break; // 水面
        float h = height(p.x, p.z);
		if (h > p.y) {
            p.y = h;
            break;
        }
		t += 0.001 + max((p.y - h) * .5, 0.001 * t);
		if (t > 10000.) break;
	}

    vec3 norm = vec3(0., 1., 0.);
    if (t < 10000.) {
        if (p.y < 0.) {
            col = vec3(.0, .5, .7);
        }
        else {
            col = vec3(.7, .7, .5);
            norm = normalize(vec3(
                height(p.x - .01, p.z) - height(p.x + .01, p.z),
                .02,
                height(p.x, p.z - .01) - height(p.x, p.z + .01)));
        }

        float value = max(0., dot(norm, lightDirection));
        col *= value;
    }

	fragColor = vec4(col, 1);
}
```

---

我们回顾在[着色器（三）](@/posts/shader_3.md)中介绍了噪声。

现在将高度函数改为：

```glsl
float random(vec2 uv) {
	return fract(sin(dot(uv.xy, vec2(11.9898, 78.233))) * 43758.5453123);
}

float perlin(vec2 st) {
    vec2 i = floor(st);
    vec2 f = fract(st);

    float a = random(i);
    float b = random(i + vec2(1.0, 0.0));
    float c = random(i + vec2(0.0, 1.0));
    float d = random(i + vec2(1.0, 1.0));

    vec2 u = smoothstep(0.0, 1.0, f);

    return mix(a, b, u.x) +
            (c - a) * u.y * (1.0 - u.x) +
            (d - b) * u.x * u.y;
}

float fbm(vec2 uv) {
    float value = 0.0;
    float amplitude = .5;
    float frequency = 0.;

    for (int i = 0; i < 10; i++) {
        value += amplitude * perlin(uv);
        uv *= 2.;
        amplitude *= .5;
    }
    return value;
}

float height(float x, float z) {
    return (fbm(vec2(x, z) * .0015) - .5) * 1000. - 20.;
}
```

---

给天空加上渐变和太阳：

```glsl
vec3 drawSky(vec3 rd) {
    vec3 c = mix(vec3(.6, .7, .9), vec3(.35, .62, 1.2), pow(max(rd.y + .15, 0.), .5));
    c += pow(max(dot(rd, lightDirection) + .0005, 0.), 3000.);
    return c;
}

// 改为
vec3 col = drawSky(rd);

// 为了看得见太阳，需要对 `ro` 与 `rd` 作一些调整
```

给水面加上高低：

```glsl
float water(float x, float z) {
    vec2 p = vec2(x, z);
    return perlin(p * 0.1) +
        perlin(p * 0.4) * 0.2 +
        perlin(p * 1.5) * 0.04 +
        perlin(p * 5.0) * 0.008;
}

// 相应位置添加
norm = normalize(vec3(water(p.x - .01, p.z) - water(p.x + .01, p.z),
    .02,
    water(p.x, p.z - .01) - water(p.x, p.z + .01)));
```

参考文中还提到高光、环境光与阴影，水面渲染，此处略去。

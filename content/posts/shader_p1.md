+++
title = "着色器（一）：基础逻辑与技巧"
description = "基于 GLSL 的着色器基础内容：2D 绚丽图像及 Raymarching."
date = 2025-09-03

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机"]
+++

参考了 [kishimisu](https://www.youtube.com/@kishimisu) 的视频教程。

以下均采取平台 [Shadertoy](https://www.shadertoy.com/new) 的配置，使用 [GLSL](https://registry.khronos.org/OpenGL-Refpages/gl4/index.php)

## 基本效果
着色器的入口函数是 `mainImage`，接受参数
* 写 `fragColor`，类型为 `vec4(r, g, b, a)`
* 读 `fragCoord`，类型为 `vec2(x, y)`

从一个简单的例子开始
```glsl
// 颜色渐变调色板函数
vec3 palette(float t, vec3 a, vec3 b, vec3 c, vec3 d) {
	return a + b * cos(6.28318*(c*t+d));
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    // 归一化，使坐标范围 -1 ~ 1
    vec2 uv = fragCoord / iResolution.xy * 2.0 - 1.0;

	// 保证图像不被拉伸
	uv.x *= iResolution.x / iResolution.y;

	// 到原点的距离
	float d = length(uv);

	// note: SDF 有向距离场：一个点到一个闭曲线的最短距离（外部为正，内部为负）
	// d = d - 0.5;

	vec3 col = palette(d + iTime,
		vec3(0.5, 0.5, 0.5),
		vec3(0.5, 0.5, 0.5),
		vec3(1.0, 1.0, 1.0),
		vec3(0.263, 0.416, 0.557));

	// 随时间变化
	d = sin(d*8. + iTime)/8.;

	d = abs(d);

	// 平滑过渡
	// d = smoothstep(0.0, 1.0, d);

	d = 0.02 / d;

	col *= d;

	fragColor = vec4(col, 1.0);
}
```

可以更进一步
```glsl
void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
	vec2 uv0 = uv;
	vec3 finalColor = vec3(0.0);

	// 迭代
	for (float i = 0.0; i < 4.0; i++) {
		// 区域重复
		// uv = fract(uv * 2.0) - 0.5;

		// 非整数引入更多变化
		uv = fract(uv * 1.5) - 0.5;

		// float d = length(uv);
		float d = length(uv) * exp(-length(uv0));

		vec3 col = palette(length(uv0) + iTime * .4);

		d = sin(d * 8. + iTime) / 8.;
		d = abs(d);

		// d = 0.02 / d;

		// 降低暗部颜色
		d = pow(0.01 / d, 1.2);

		finalColor += col * d;
	}

	fragColor = vec4(finalColor, 1.0);
}
```

## Raymarching
一种 3D 技术是将场景分割成大量的三角形，渲染的主要流程为：
1. vertex shader 将 3D 坐标转化为画布坐标
2. rasterization 光栅化，判断三角形覆盖哪些像素
3. fragment shader 给这些像素填色

另一种技术是 ray-marching，使用步进的方法让光线行进直到击中对象（不同于直接求交的 ray-tracing），由于巨大的计算开销，通常用于渲染体积云、局部光照和数学模型。

可以参考 [Shadertoy: Raymarching in Raymarching](https://www.shadertoy.com/view/wlSGWy) 的演示，以及这个关于[有向距离函数](https://iquilezles.org/articles/distfunctions/)的文章。

从一个最简单的例子开始
```glsl
// Sphere SDF
float sdSphere(vec3 p, float s) {
	return length(p) - s;
}

// distance to scene
float map(vec3 p) {
	vec3 spherePos = vec3(sin(iTime) * 3., 0, 0);
	float sphere = sdSphere(p - spherePos, 1.);
	return sphere;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
	vec2 uv = (fragCoord * 2. - iResolution.xy) / iResolution.y;

	vec3 ro = vec3(0, 0, -3); // ray origin
	vec3 rd = normalize(vec3(uv, 1)); // ray direction
	vec3 col = vec3(0);

	float t = 0.; // distance travelled

	for (int i = 0; i < 80; i++) {
		vec3 p = ro + rd * t; // position
		float d = map(p);
		t += d;

		// col = vec3(i) / 80.;

		if (d < .001 || t > 100.) break; // early stop
	}

	col = vec3(t * .2);

	fragColor = vec4(col, 1);
}
```

可以继续添加其它形状，并组合
```glsl
float opUnion(float d1, float d2)
{
    return min(d1, d2);
}
float opSubtraction(float d1, float d2)
{
    return max(-d1, d2);
}
float opIntersection(float d1, float d2)
{
    return max(d1, d2);
}
float opXor(float d1, float d2)
{
    return max(min(d1, d2), -max(d1, d2));
}
```

以及平滑过渡等
```glsl
float opSmoothUnion(float d1, float d2, float k)
{
    float h = clamp(0.5 + 0.5*(d2-d1)/k, 0.0, 1.0);
    return mix(d2, d1, h) - k*h*(1.0 - h);
}

float opSmoothSubtraction(float d1, float d2, float k)
{
    float h = clamp(0.5 - 0.5*(d2+d1)/k, 0.0, 1.0);
    return mix(d2, -d1, h) + k*h*(1.0 - h);
}

float opSmoothIntersection(float d1, float d2, float k)
{
    float h = clamp(0.5 - 0.5*(d2-d1)/k, 0.0, 1.0);
    return mix(d2, d1, h) + k*h*(1.0 - h);
}
```

缩放、旋转可以通过数学方法得到。
```glsl
// float box = sdBox(q * 4., vec3(.75)) / 4.;

mat2 rot2D(float angle) {
	float s = sin(angle);
	float c = cos(angle);
	return mat2(c, -s, s, c);
}

// p.xy *= rot2D(iTime);

vec3 rot3D(vec3 p, vec3 axis, float angle) {
	// Rodrigues' rotation formula
	return mix(dot(axis, p) * axis, p, cos(angle))
		+ cross(axis, p) * sin(angle;)
}
```

使用全局变量 `iMouse` 可以实现随鼠标旋转的效果。

使用 `mod(x, 1.0) = fract(x)` 可以实现空间复制的效果。

+++
title = "着色器（三）：随机与噪声"
description = "基于 GLSL 的噪声实现及应用。"
date = 2025-10-01
updated = 2025-10-30

[extra]
toc = true
math = true

[extra.cover]
image = "images/cover/shader_pencil_effect.png"

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "计算机图形学"]
+++

参考了 [The Book of Shaders](https://thebookofshaders.com/) 的主题 [Random](https://thebookofshaders.com/10/)，[Noise](https://thebookofshaders.com/11/)，[Cellular noise](https://thebookofshaders.com/12/) 及 [Fractional brownian motion](https://thebookofshaders.com/13/).

## 随机
### 原理
由于 GLSL 并不能从物理世界获得熵源，随机都是伪随机。

但我们并不需要真随机，只需要看起来有随机性即可。

{% admonition(type="tip", title="提示") %}
真随机与均匀随机是不同的概念。
{% end %}

一个简洁的生成方法是
```glsl
float res = fract(sin(x) * 100000.0);
```

在 2D 情形下，需要采取不可约的系数（读者可自行测试 `col = vec3(fract(sin(uv.x) * 100000.0 + sin(uv.y) * 100.0));` 的结果）
```glsl
float random(vec2 uv) {
	return fract(sin(dot(uv.xy, vec2(11.9898, 78.233))) * 43758.5453123);
}

vec2 random2(vec2 uv) {
    return fract(sin(vec2(dot(uv, vec2(127.1, 311.7)), dot(uv, vec2(269.5, 183.3)))) * 43758.5453);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord/iResolution.xy;
    vec3 col = vec3(random(uv));
    fragColor = vec4(col, 1.0);
}
```

### 应用技巧
可以把一个系数调得很小，产生在该方向上长条形的效果。

{% admonition(type="tip", title="提示") %}
如果你希望保持长条形的效果但将它改为曲线，则不应在浮点数上操作，而是在原来的像素整点上操作到整点再使用。
{% end %}

[Random](https://thebookofshaders.com/10/) 中列举了很多应用技巧，其中包括一个迷宫生成器，整理如下：

```glsl
#define PI 3.14159265358979323846

float random(vec2 uv) { ... }

// truchet tiling
vec2 transform(vec2 uv, float rnd){
    if (rnd > 0.5) uv.x = 1.0 - uv.x;
    if (mod(rnd, 0.5) > 0.25) uv.y = 1.0 - uv.y;
    return uv;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.y * 20.0;
    vec2 tile = transform(fract(uv), random(floor(uv)));
    float color = abs(tile.x - tile.y) < 0.1 ? 1.0 : 0.0;
    fragColor = vec4(vec3(color), 1.0);
}
```

## 噪声
上述的随机生成确实保证了随机性，类似于白噪声，但我们通常希望看到更平滑的图像。

噪声是算法生成的、具有随机性的图像，可以用于生成地形、纹理。

### 2D 噪声
我们来看 Perlin 噪声 的 2D 版本的代码：
```glsl
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
```

这里 `mix` 函数用于插值，表达式为 `mix(x, y, a) = x * (1 - a) + y * a`

整个函数的思路是：划分成一个个单元格，格的顶点被赋予随机的值，而后格内部进行平滑的插值。

但是现在看起来有明显的方形轮廓。

对此，有以下优化思路：
* 对格的顶点赋予向量值，做两次插值。
* 另外可以进行优化：将 `smoothstep` 中调用的 $\mathrm{lerp}(x) = 3x^2-2x^3$ 改为 $6x^5-15x^4+10x^3$.
* 进行局部随机化。

### 应用技巧
封面图的代码如下，使用了局部随机化（这产生了蜡笔效果），并引入了时间维度：
```glsl
vec3 hsv2rgb(vec3 c) { ... }

float random(vec2 uv) {
	return fract(sin(dot(uv.xy, vec2(11.143, 78.233))) * (43758.5453123 + iTime));
}

float perlin(vec2 st) { ... }

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
    vec2 uv2 = vec2(uv.x * 20.0, uv.y * 1000.0);
    float fr = perlin(uv2 + random(uv2));

    float t = smoothstep(0.0, 1.0, fr);
    vec3 col = t * hsv2rgb(vec3((1.0 - uv.y) / 2.0, 1.0, 1.0)) + (1.0 - t) * vec3(1.0);
    fragColor = vec4(col, 1.0);
}
```

[Noise](https://thebookofshaders.com/11/) 中列举了更多应用实例。

### 时间优化
Simplex 噪声是对 Perlin 噪声的优化（在高维情形下，Perlin 的时间复杂度是 $O(2^n)$）。

我们以单形为晶格（在 2D 中是三角形，在 3D 中是四面体）。

这些单形是从立方晶格中裁得的。

## 元胞噪声
### 原理
元胞噪声可以产生类似细胞的效果。

对每个点，可以赋予一个距离场，例如取为到给定四点的距离的最小值。

### Worley 噪声
现在对每个点，我们考虑它所在的方格及周围的 8 个方格。

我们随机地在每个方格中取一个固定的点，即可使用前述方法。

```glsl
vec2 random2(vec2 uv) { ... }

float worley(vec2 uv) {
	vec2 grid = floor(uv);
    vec2 uv2 = fract(uv);
    float m_dist = 1.;
    for (int y= -1; y <= 1; y++) {
        for (int x= -1; x <= 1; x++) {
            vec2 neighbor = vec2(float(x), float(y));
            vec2 point = random2(grid + neighbor);
            float dist = length(neighbor + point - uv2);
            m_dist = min(m_dist, dist);
        }
    }
    return m_dist;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.y * 8.0;
    fragColor = vec4(vec3(worley(uv)), 1.0);
}
```

### Voronoi 噪声
改为生成直边界。

这是通过记录最近的点。

```glsl
float voronoi(vec2 uv) {
    vec2 grid = floor(uv);
    vec2 uv2 = fract(uv);
    float m_dist = 1.;
    vec2 closest;
    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            vec2 neighbor = vec2(float(x), float(y));
            vec2 point = random2(grid + neighbor);
            // point = 0.5 + 0.5 * sin(iTime + 6. * point);
            float dist = length(neighbor + point - uv2);
            if (dist < m_dist) {
                m_dist = dist;
                closest = point;
            }
        }
    }
    return closest.x;
}
```

## 分形化
### 分形 Brownian 运动
Fractional Brownian Motion 一般被表为：

$$\mathrm{fbm}(x, y) = \sum_{i=1}^n w^i \cdot \mathrm{noise}(s^ix, s^iy)$$

其中取 $w=\frac{1}{2}, s=2$，此时称每次迭代为 octave.

有以下例子：
```glsl
float random(vec2 uv) { ... }

float perlin(vec2 uv) { ... }

float fbm (vec2 uv) {
    float value = 0.0;
    float amplitude = .5;
    float frequency = 0.;

    for (int i = 0; i < 6; i++) {
        value += amplitude * perlin(uv);
        uv *= 2.;
        amplitude *= .5;
    }
    return value;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 uv = fragCoord.xy / iResolution.y * 8.0;
    fragColor = vec4(vec3(fbm(uv * 3.0)), 1.0);
}
```

更多内容可参考 Inigo Quilez 的经典作品 [Rainforest](https://www.shadertoy.com/view/4ttSWf).

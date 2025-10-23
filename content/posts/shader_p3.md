+++
title = "【草稿】着色器（三）：随机与噪声"
description = "基于 GLSL 的噪声实现及应用。"
date = 2025-10-01
updated = 2025-10-16

[extra]
toc = true
math = true

[extra.cover]
image = "images/cover/shader_pencil_effect.png"

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["杂物"]
tags = ["数学", "计算机", "计算机图形学"]
+++

参考了 [The Book of Shaders](https://thebookofshaders.com/)

## 随机
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

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord/iResolution.xy;
    vec3 col = vec3(random(uv));
    fragColor = vec4(col, 1.0);
}
```

这种随机结果的更多应用参见 <https://thebookofshaders.com/10/>

可以把一个系数调得很小，产生在该方向上长条形的效果。

{% admonition(type="tip", title="提示") %}
如果你希望保持长条形的效果但将它改为曲线，则不应在浮点数上操作，而是在原来的像素整点上操作到整点再使用。
{% end %}

## 噪声
上述的随机生成确实保证了随机性，类似于白噪声，但我们通常希望看到更平滑的图像。

噪声是算法生成的、具有随机性的图像，可以用于生成地形、纹理。

### Perlin 噪声
我们来看 2D 版本的代码：
```glsl
float noise (vec2 st) {
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

封面图的代码如下，使用了局部随机化，并引入了时间维度：
```glsl
vec3 hsv2rgb(vec3 c) { ... }

float random(vec2 uv) {
	return fract(sin(dot(uv.xy, vec2(11.143, 78.233))) * (43758.5453123 + iTime));
}

float noise (vec2 st) { ... }

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
    vec2 uv2 = vec2(uv.x * 20.0, uv.y * 1000.0);
    float fr = noise(uv2 + random(uv2));

    float t = smoothstep(0.0, 1.0, fr);
    vec3 col = t * hsv2rgb(vec3((1.0 - uv.y) / 2.0, 1.0, 1.0)) + (1.0 - t) * vec3(1.0);
    fragColor = vec4(col, 1.0);
}
```

### Simplex 噪声
这是对 Perlin 噪声的优化（在高维情形下，Perlin 的时间复杂度是 $O(2^n)$）。

我们以单形为晶格（在 2D 中是三角形，在 3D 中是四面体）。

这些单形是从立方晶格中裁得的。

### Worley 噪声
Voronoi/Worley/Cell 噪声可产生类似细胞的效果。

类似于 Perlin，仍然给每个格点赋值一个向量，但是是作为它的偏移。

当我们判断一个点在哪个细胞中时，我们进行 3×3 的采样，分别计算偏移后的坐标，找到其中最近的那个。

## 技巧
### 分形化
考虑 Diffusion-limited Aggregation

{{ todo() }}

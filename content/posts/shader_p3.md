+++
title = "着色器（三）：随机与噪声"
description = "基于 GLSL 的噪声实现及应用。"
date = 2025-10-01

[extra]
toc = true
math = true

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
	return fract(sin(dot(uv.xy, vec2(11.9898,78.233))) * 43758.5453123);
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = fragCoord/iResolution.xy;
    vec3 col = vec3(random(uv));
    fragColor = vec4(col, 1.0);
}
```

这种随机结果的更多应用参加 <https://thebookofshaders.com/10/>

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

但是现在看起来有明显的方形轮廓。可以进行优化：对格的顶点赋予向量值，做两次插值。

另外可以进行优化：将 `smoothstep` 中调用的 $\mathrm{lerp}(x) = 3x^2-2x^3$ 改为 $6x^5-15x^4+10x^3$

### Simplex 噪声
这是对 Perlin 噪声的优化。我们对三角形差值（在 3D 中是四面体）

### Worley 噪声
{{ todo() }}

## 技巧
### 分形化
{{ todo() }}

### 纹理
{{ todo() }}

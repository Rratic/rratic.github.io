+++
title = "着色器（二）：色彩与复杂绘制"
description = "基于 GLSL 的 HSV 操作与经典数学对象的绘制。"
date = 2025-09-22

[extra]
toc = true
math = true

[extra.cover]
image = "images/cover/shader_exp_inv.png"

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "计算机", "计算机图形学"]
+++

## 色彩
### HSV
HSV 是一种比较好的适用于人类视觉的颜色模型。

一个颜色包含了
* 色相（Hue）的范围是 0° ~ 360°，其中 0° 为红，120° 为绿，240° 为蓝
* 饱和度（Saturation）的范围是 0% ~ 100%
* 亮度（Value）的范围是 0% ~ 100%

在后文中，我们都归一到范围 0 ~ 1 去处理。

GLSL 中参数是 RGB 的，因此需要将 HSV 转为 RGB，可参考 Sam Hocevar 的代码。（可以在 <https://github.com/hughsk/glsl-hsv2rgb> 找到它的一个 npm package）
```glsl
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
```

### 色板生成算法
参考这个帖子：[Ant Design 色板生成算法演进之路](https://zhuanlan.zhihu.com/p/32422584)，可以在 [Ant Design 的色板互动页面](https://ant.design/docs/spec/colors-cn) 体验其效果。

将其中的 Javascript 代码翻译为 GLSL 如下：
```glsl
#define hueStep (1.0 / 180.0)
float getHue (float h, float i) {
	if (h >= 1.0 / 6.0 && h <= 2.0 / 3.0) { // 冷色调
		// 减淡变亮 色相顺时针旋转 更暖
		// 加深变暗 色相逆时针旋转 更冷
		return h + hueStep * i;
	}

	// 暖色调
	// 减淡变亮 色相逆时针旋转 更暖
	// 加深变暗 色相顺时针旋转 更冷
	return h - hueStep * i;
}

float getSaturation (float s, float i) {
	// 减淡变亮 饱和度迅速降低
	if (i <= 0.0) {
		return s + 0.16 * i;
	}

	// 加深变暗-最暗 饱和度提高
	if (i >= 4.0) {
		return s + 0.16;
	}

	// 加深变暗 饱和度缓慢提高
	return s + 0.05 * i;
}

float getValue (float v, float i) {
	if (i <= 0.0) {
		return v - 0.05 * i;
	}

	return v - 0.15 * i;
}

vec3 palette (vec3 col, float i) {
	return hsv2rgb(vec3(
		getHue(col.x, i),
		getSaturation(col.y, i),
		getValue(col.z, i)
	));
}
```

### 光照
使用 HSV 可以给[上一篇提及的 Raymarching](@/posts/shader_1.md#Raymarching) 渲染的曲面添加光照效果。

一个简单的方法是将光照方向与曲面在该点处的法方向的余弦值赋予给亮度。

## 数学绘制
### 示例
以下代码绘制的是 Poincaré 圆盘模型中，上半平面模型的格线的对应。

其想法很简单：算出逆映射并模拟。
```glsl
void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
    
    float l2 = uv.x * uv.x + uv.y * uv.y;
	
    if (l2 > 1.0) {
        fragColor = vec4(0.0, 0.0, 0.0, 1.0);
        return;
    }
 
    uv.y += 1.0;

    l2 = uv.x * uv.x + uv.y * uv.y;

    vec2 uv2 = uv / l2;

    float dest1 = fract(uv2.x);
    float dest2 = fract(uv2.y);

    fragColor = vec4(dest1, dest2, 1.0 - dest1 * dest2, 1.0);
}
```

### 变换
图形的基础变换（平移、旋转、缩放）是容易完成的。

一个好的例子是 [Oblivion radar](https://www.shadertoy.com/view/4s2SRt). 这里包含了一些技巧：

对 2D 场景来说，不重合的图形可以直接不断叠加颜色值。
```glsl
vec3 finalColor;
finalColor += ...
finalColor += ...
```

指针的曳尾效果部分代码整理为：
```glsl
float movingLine(vec2 d, float radius) {
    float theta0 = -1.2 * iTime;
    float r = length(d);
    if (r < radius) {
        // 计算点 d 到 θ0 角度线的距离，但钳制为劣角
        vec2 p = radius * vec2(cos(theta0), sin(theta0));
        float l = length(d - p * clamp(dot(d, p) / dot(p, p), 0.0, 1.0));

        // 计算角度之差并钳制角的大小
   	 	float theta = mod(atan(d.y, d.x) - theta0, 2.0 * PI);
        float gradient = clamp(1.0 - 1.2 * theta, 0.0, 1.0);

        // 亮色部分主要利用了 smoothstep 中调用了 clamp
        return (1.0 - smoothstep(0.0, 2.0, l)) + 0.5 * gradient;
    }
    else return 0.0;
}
```

### 复函数
参考 Elias Wegert 的 *Visual Complex Functions* 的想法（可以在 [Domain coloring](https://complex-analysis.com/content/domain_coloring.html) 看到大量函数绘制结果），可以用色相来表示值的辐角，用亮度表示值的模长。

渲染 $z\mapsto e^{\frac{1}{z}}$ 的代码如下：
```glsl
precision highp float; // 设置为最高精度

#define PI 3.141592653589793

vec3 hsv2rgb(vec3 c) { ... }

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
    uv = uv / (0.1 + iTime);
    uv = vec2(uv.x, -uv.y) / (uv.x * uv.x + uv.y * uv.y);
    vec2 val = vec2(exp(uv.x) * cos(uv.y), exp(uv.x) * sin(uv.y));
    float len = 0.5 + 0.5 * exp(fract(log(length(val)) * 2.0) - 1.0);
    float rot = fract((uv.y / PI + 1.0) / 2.0);
    vec3 col = vec3(rot, 1.0, len);
    fragColor = vec4(hsv2rgb(col), 1.0);
}
```

### 分形
GLSL 不支持递归（因为需要支持不支持递归的硬件），但是我们仍然可以使用循环。

一般的思路是：找到一个适合递归的结构，然后作坐标转换。

以下为 Kech 雪花的示例。
```glsl
// 一条 (0, 0) 到 (1, 0) 的边，向上方延伸
float kech_edge(float x, float y) {
    float d = s3;
    for (int i = 0; i < DEPTH; i++) {
        if (x > 0.5) x = 1.0 - x;
        if (y > x / s3) return 1.0;
        if (x < 1.0 / 3.0) {
            x *= 3.0;
            y *= 3.0;
        }
        else {
            x -= 1.0 / 3.0;
            float y0 = (y - s3 * x) / 2.0;
            if (y0 <= 0.0) {
                return y0 / d;
            }
            float x0 = sqrt(x * x + y * y - y0 * y0);
            x = x0 * 3.0;
            y = y0 * 3.0;
        }
        d *= s3;
    }
    return 1.0;
}
```

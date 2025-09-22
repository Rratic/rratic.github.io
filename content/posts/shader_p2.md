+++
title = "着色器（二）：色彩与复杂绘制"
description = "基于 GLSL 的 HSV 操作与经典数学对象的绘制。"
date = 2025-09-22

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["杂物"]
tags = ["数学", "计算机", "计算机图形学"]
+++

## 色彩
### HSV
HSV 是一种比较好的适用于人类视觉的颜色模型。

一个颜色包含了
* 色相（Hue）的范围是 0° ~ 360°，其中 0° 为红，120° 为绿，240° 为蓝
* 饱和度（Saturation）的范围是 0% ~ 100%
* 亮度（Value）的范围是 0% ~ 100%

在以下讨论中，我们都归一到范围 0 ~ 1

GLSL 中参数是 RGB 的，因此需要将 HSV 转为 RGB，可参考 Sam Hocevar 的代码。（可以在 <https://github.com/hughsk/glsl-hsv2rgb> 找到它的一个 npm package）
```glsl
vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
```

### 色板生成算法
[Ant Design 的色板互动页面](https://ant.design/docs/spec/colors-cn)

参考这个帖子：[Ant Design 色板生成算法演进之路](https://zhuanlan.zhihu.com/p/32422584)。

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

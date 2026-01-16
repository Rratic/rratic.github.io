+++
title = "着色器（四）：光线追踪原理"
description = "基于 GLSL 的噪声实现及应用。"
draft = true

[extra]
toc = true
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "图形学"]
+++

参考了 <https://www.zhihu.com/question/263415001/answer/125678744941>.

## 光强
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

+++
title = "范畴论知识索引"
description = "按学习历程排布的范畴论相关文章及符号表。"
date = 2025-07-22

[extra]
math = true

[extra.sitemap]
priority = "0.6"

[taxonomies]
categories = ["项目"]
tags = ["索引", "数学", "范畴论"]
+++

## 主线
参考阅读 Tom Leinster 的 *Basic Category Theory*，Steve Awodey 的 *Category Theory*（尚未找到中译本）及李文威的《代数学方法：卷一》

### 范畴与态射
- 基本的概念
	- 定义（范畴，对象 $\operatorname{Ob}(\mathcal{C})$，态射 $\operatorname{Mor}(\mathcal{C}), \operatorname{Hom}_{\mathcal{C}}(X, Y), X\xrightarrow{f}Y$，来源/域 $s, \operatorname{dom}$，目标/陪域 $t, \operatorname{cod}$，复合 $\circ$，恒等态射 $1_A$）
	- 自同态 $\operatorname{End}_{\mathcal{C}}(X)$
	- 子范畴
- 常见的范畴（集合与函数 $\mathrm{Sets}$，偏序集与单调函数 $\mathrm{Pos}$，关系 $\mathrm{Rel}$，有限范畴，离散范畴）
- 构造（积 $\times, \prod$，对偶 $\mathcal{C}^{op}$，箭头 $\mathcal{C}^\to$，切片 $\mathcal{C}/C$，余切片 $C/\mathcal{C}$）
- 自由范畴（Kleene 闭包）
- 技术细节（大、小、局部小）

### 函子与自然变换
- 函子（协/共变函子，反变函子）
- 同构（函子间同构 $\stackrel{\sim}{\to}$，拟逆函子，同构 $\cong, \operatorname{Isom}_{\mathcal{C}}(X, Y)$，逆，Cayley 定理）
	- 自同构 $\operatorname{Aut}_{\mathcal{C}}(X)$
- 忘性函子
- 自然变换（$F\Longrightarrow G$，横，纵合成）
- 泛性质的始对象表述（始对象 $0$，终对象 $1$）

### 极限
- 等化子，余等化子
- 余积（$\oplus, \coprod$），拉回，推出：见[**范畴论（一）**](/posts/category-theory-p1/)
- 极限，余极限

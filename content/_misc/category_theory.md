+++
title = "#范畴论"

[extra]
math = true

[taxonomies]
tags = ["数学", "范畴论"]
+++

参考阅读
* Tom Leinster *Basic Category Theory*
* Steve Awodey *Category Theory*
* *Categories for the Working Mathematician* (GTM 5)
* 李文威《代数学方法：卷一》

## 范畴与态射
- 基本的概念
	- 定义（范畴，对象 $\operatorname{Ob}(\mathcal{C})$，态射 $\operatorname{Mor}(\mathcal{C}), \operatorname{Hom}_{\mathcal{C}}(X, Y), X\xrightarrow{f}Y$，来源/域 $s, \operatorname{dom}$，目标/陪域 $t, \operatorname{cod}$，复合 $\circ$，恒等态射 $1_A$）
	- 自同态 $\operatorname{End}_{\mathcal{C}}(X)$
	- 子范畴
- 常见的范畴（集合与函数 $\mathrm{Sets}$，偏序集与单调函数 $\mathrm{Pos}$，关系 $\mathrm{Rel}$，有限范畴，离散范畴）
- 构造（积 $\times, \prod$，对偶 $\mathcal{C}^{op}$，箭头 $\mathcal{C}^\to$，切片 $\mathcal{C}/C$，余切片 $C/\mathcal{C}$）
- 自由范畴（Kleene 闭包）
- 单态射 $\rightarrowtail$，满态射 $\twoheadrightarrow$
- 技术细节（大、小、局部小）

### 函子与自然变换
- 函子（协/共变函子，反变函子）
- 同构（函子间同构 $\stackrel{\sim}{\to}$，拟逆函子，同构 $\cong, \operatorname{Isom}_{\mathcal{C}}(X, Y)$，逆，Cayley 定理）
	- 自同构 $\operatorname{Aut}_{\mathcal{C}}(X)$
- 忘性函子，对角函子
- 自然变换（$F\Longrightarrow G$，横，纵合成）
- 泛性质的始对象表述（始对象 $0$，终对象 $1$）

### 极限
- 余积（$\oplus, \coprod$），拉回，推出，等化子，余等化子 ⇒ [**范畴论（一）**](/posts/category-theory-p1/)
- 极限，余极限

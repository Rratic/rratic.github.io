+++
title = "【逻辑学】模态逻辑及其应用"
date = 2026-04-27

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "逻辑学"]
+++

发展出（命题）模态逻辑的动机来自于，命题逻辑的实质蕴涵并不总是与自然语言的“如果……那么……”完全相符，因为自然语言中还携带了因果、时间、规范、认知、反事实等信息。由于精力有限，本文涉及的内容会远少于对应的课程讲义。

<!-- more -->

## 形式语言
模态逻辑提供的是各种不同模态概念（知识逻辑、道义逻辑、时态逻辑等）的统一范式。

{% admonition(type="definition", title="命题模态逻辑语言 ML") %}
对字母集合 $\mathbf{P}$, 语言 ML 的公式由以下规则归纳生成：
- 原子公式：每个 $p \in \mathbf{P}$ 是公式
- 否定：若 $\varphi$ 是公式，则 $\neg \varphi$ 是公式
- 合取：若 $\varphi, \psi$ 是公式，则 $\varphi \wedge \psi$ 是公式
- 必然算子：若 $\varphi$ 是公式，则 $\Box \varphi$ 是公式
{% end %}

这里用 `Box` 表示那个模态词。我们再定义：

$$\Diamond \varphi \coloneqq \neg \Box \neg \varphi$$

是 `Box` 的对偶。如，当 `Box` 的含义是“必然”（真势模态）时，`Diamond` 的含义是“可能”。

从直观上我们可以用自然语言理解：
- $\neg \Diamond p$ 意为“不可能 $p$”
- $\Diamond p \wedge \Diamond \neg p$ 意为“$p$ 可能真，也可能假”
- $p \wedge \neg \Box p$ 意为“$p$ 偶然真”

对“包含模态词的自然语言”来说，一个歧义来源是辖域，即模态词作用范围。

> 如果你只吃了一个馒头，那么你*必然*吃的馒头数量少于两个。

这有两种形式化方法，即窄辖域读法 $p \to \Box q$ 与宽辖域读法 $\Box (p \to q)$. 似乎宽辖域读法更符合本意，而窄辖域读法等价于 $\neg \Box q \to \neg p$ 不合理。

## 形式语义
### Kripke 模型
模态逻辑最自然的语义是可能世界语义。

一个 Kripke 模型 $\mathcal{M}$ 包含：
- 可能世界集 $W$ 是非空集合，表示“可能的世界”
- 可达关系 $R \subseteq W \times W$ 是二元关系，可以视作以 $W$ 为顶点的图的有向边
- 赋值函数 $V$ 指出每个世界中哪些命题字母为真

指定世界 $w$ 的原子命题、否定、合取的满足关系是自然的。而我们说 $\mathcal{M}, w \models \Box \varphi$ 当且仅当对所有 $w$ 可达的世界 $u$, $\varphi$ 在 $u$ 上为真。

现在，我们可以有严格蕴涵 $\varphi ⊰ \psi \coloneqq \Box (\varphi \to \psi)$. 这要强于实质蕴涵从而解决它的怪论，尽管它自身仍存在怪论（无法表达相关性）。

我们称框架 $\mathcal{F} = \braket{W, R}$ 是去掉赋值函数后的模型骨架。有效性有多个层级：在模型上有效 $\mathcal{M} \models \varphi$; 在点框架上有效 $\mathcal{F}, w \models \varphi$; 在框架上有效 $\mathcal{F} \models \varphi$; 框架类（一类框架）有效 $\Complex \models \varphi$; 有效 $\models \varphi$.

### 性质对应
| 名称 | 模态公式 | 框架中关系的性质 |
| :-: | :-: | :-: |
| 自反性（T 公理） | $\Box p \to p$ | $\forall x (xRx)$ |
| 持续性（D 公理） | $\Box p \to \Diamond p$ | $\forall x \exists y (xRy)$ |
| 对称性（B 公理） | $p \to \Box \Diamond p$ | $\forall x \forall y (xRy \to yRx)$ |
| 传递性（4 公理） | $\Box p \to \Box \Box p$ | $\forall x \forall y \forall z ((xRy \wedge yRz) \to xRz)$ |
| 稠密性 | $\Diamond p \to \Diamond \Diamond p$ | $\forall x \forall y (xRy \to \exists z (xRz \wedge zRy))$ |
| 合流性 | $\Diamond \Box p \to \Box \Diamond p$ | $\forall x \forall y \forall z ((xRy \wedge xRz) \to \exists t (yRt \wedge zRt))$ |
| 欧性（5 公理） | $\Diamond p \to \Box \Diamond p$ | $\forall x \forall y \forall z ((xRy \wedge xRz) \to yRz)$ |
| 逆良基（Löb 条件） | $\Box (\Box p \to p) \to \Box p$ | 传递且没有无穷下降链[^chain] |

{% admonition(type="question", title="习题") %}
证明稠密性的对应关系，即：公式 $\Diamond p \to \Diamond \Diamond p$ 在 $\mathcal{F}$ 上有效当且仅当 $\mathcal{F}$ 满足：

$$\forall x \forall y (xRy \to \exists z (xRz \wedge zRy))$$
{% end %}

先证充分性（设 $\mathcal{F}$ 满足上式）：对任意赋值 $V$ 与世界 $x$, 若 $\mathcal{M}, x \models \Diamond p$, 则存在 $y$ 使得 $xRy$ 且 $\mathcal{M}, y\models p$. 由稠密性，存在 $z$ 使得 $xRz$ 且 $zRy$, 从而 $\mathcal{M}, x \models \Diamond \Diamond p$. 因此 $\mathcal{M}, w \models \Diamond p \to \Diamond \Diamond p$. 由任意性知公式有效。

再证必要性。设 $\mathcal{F}$ 不满足上式，则存在世界 $x, y \in W$ 使得 $xRy$ 且不存在 $z$ 满足 $xRz \wedge zRy$. 在该框架上定义赋值 $V(p) = \set{y}$, 有 $\mathcal{M}, x \models \Diamond p$, 但无法满足 $\mathcal{M}, x \models \Diamond \Diamond p$. 故 $\mathcal{M}, x \nvDash \Diamond \Diamond p$, 即 $\mathcal{M}, x \nvDash \Diamond p \to \Diamond \Diamond p$. 该公式在 $\mathcal{F}$ 上不有效，逆否命题成立。

{% admonition(type="question", title="习题") %}
证明合流性的对应关系，即：公式 $\Diamond \Box p \to \Box \Diamond p$ 在 $\mathcal{F}$ 上有效当且仅当 $\mathcal{F}$ 满足：

$$\forall x \forall y \forall z ((xRy \wedge xRz) \to \exists t (yRt \wedge zRt))$$
{% end %}

先证充分性（设 $\mathcal{F}$ 满足上式）：对任意赋值 $V$ 与世界 $x$, 若 $\mathcal{M}, x \models \Diamond \Box p$, 则存在 $y$ 使得 $xRy$ 且 $\mathcal{M},y\models\Box p$. 任取世界 $z$ 满足 $xRz$, 由合流性存在 $t$ 使得 $yRt \wedge zRt$. 因 $\mathcal{M}, y \models \Box p$, 得 $\mathcal{M}, t \models p$, 于是 $\mathcal{M}, z \models \Diamond p$. 由 $z$ 的任意性，$\mathcal{M}, x \models \Box \Diamond p$. 故 $\mathcal{M}, x \models \Diamond \Box p \to \Box \Diamond p$. 由任意性知公式有效。

再证必要性。设 $\mathcal{F}$ 不满足上式，则存在世界 $x, y, z$ 满足 $xRy \wedge xRz$ 且不存在 $t$ 满足 $yRt \wedge zRt$. 在该框架上定义赋值 $V(p) = \set{u \in W | yRu}$. 读者易见公式在 $\mathcal{F}$ 上不有效，逆否命题成立。

## 公理系统
### 极小系统
正规模态逻辑的最小系统 K 包含以下公理模式与规则（对应地我们对可达关系不作任何假设）：
- 命题逻辑的公理模式
- K 公理 $\Box (\varphi \to \psi) \to (\Box \varphi \to \Box \psi)$
- MP 由 $\varphi$ 与 $\varphi \to \psi$ 推出 $\psi$
- NEC 必然化规则：若 $\vdash \varphi$ 则 $\vdash \Box \varphi$

{% admonition(type="question", title="习题") %}
在 K 系统中证明：

$$(\Box p \wedge \Box q) \to \Box (p \wedge q)$$
{% end %}

我们有重言式：

$$p \to (q \to (p \wedge q))$$

使用必然化规则，然后用 K 公理即可。

## 应用
### 知识逻辑
知识逻辑的现代起点是 Jaakko Hintikka 1962 年的著作 *Knowledge and Belief: An Introduction to the Logic of the Two Notions*.

给定主体集 $\mathrm{Ag} = \set{1, 2, \cdots, n}$, 知识逻辑中的算子是 $K_i, i \in \mathrm{Ag}$. 我们可以将 $K_i$ 算子嵌套，如 $K_1 \neg K_2p$ 1 知道 2 不知道 $p$.

多主体知识模型有 $R_i \subseteq W \times W$ 表示的是主体 $i$ 的“认知不可区分”关系，即 $wR_iv$ 表示 $i$ 无法区分自己在 $w$ 还是 $v$ 中。

最理想的知识模型要求每个 $R_i$ 是等价关系。

{% admonition(type="question", title="习题") %}
是否可能出现这样的情况：成立 $\neg K_b K_a p$ 与 $K_b \neg K_a \neg K_b K_a p$.
{% end %}

不可能。假设在世界 $w$ 上同时成立，则存在 $w$ 的 $b$-可达世界 $u$ 满足 $\neg K_a p$.

另一方面 $u$ 满足 $\neg K_a \neg K_b K_a p$, 它存在 $a$-可达世界 $v$ 满足 $K_b K_a p$. 有 $v$ 上 $K_a p$, 从而所有 $u$ 的 $a$-可达世界满足 $p$ 与 $K_a p$. 矛盾。

另一种证法是用形式语言（来自我的同学）：
1. 由 T 公理 $\vdash K_b K_a p \to K_a p$
2. 由命题逻辑 $\vdash \neg K_a p \to \neg K_b K_a p$
3. 使用 NEC $\vdash K_a (\neg K_a p \to \neg K_b K_a p)$
4. 使用 K 公理 $\vdash K_a \neg K_a p \to K_a \neg K_b K_a p$
5. 由 5 公理 $\vdash \neg K_a p \to K_a \neg K_a p$, 故 $\vdash \neg K_a p \to K_a \neg K_b K_a p$
6. 故 $\vdash \neg K_a \neg K_b K_a p \to K_a p$
7. 使用 NEC 再使用 K 公理 $K_b \neg K_a \neg K_b K_a p \to K_b K_a p$

---

[^chain]: 无法用一阶逻辑表达。

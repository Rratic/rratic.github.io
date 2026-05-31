+++
title = "【逻辑学】一阶逻辑及其应用"
date = 2026-05-09
updated = 2026-05-19

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "逻辑学"]
+++

命题逻辑对推理的分析无法满足我们的需求，我们还需要对“所有”这样的词语分析。本文不涉及元定理的证明。

<!-- more -->

## 形式语言
在数学中，**名字**（name）有三类：
- **变元**（variable）使用字母表示，在日常生活中不存在完全对应的语言现象，类似的**指示词**（indexical）“这块黑板”
- **专名**（proper name）是完全确定的指称，如“0”、“苏格拉底”
- **限定摹状词**（definite description）如“$x^2$”、“那块黑板上的裂痕”

**谓词**（predicate）是有空位的表达式，如“……大于……”。

在数学中，主要关心的**量词**（quantifier）是最极端的（由于变元、等词的辅助，“唯一一个”之类的量词不是必需的）：
- **全称量词**（universal quantifier）：对于任意一个 $x$
- **存在量词**（existential quantifier）：存在一个 $x$

对于一阶形式语言，会有以下符号：
- 个体变元：各种字母等
- 等词符号：$\dot{=}$
- 命题联结词符号：$\neg, \wedge, \vee, \to$
- 量词符号：$\forall, \exists$
- 技术符号：$()$ 与 $,$

另外还会有一些非逻辑符号：常元符号、函数符号与关系符号。

{% admonition(type="definition", title="项（term）") %}
对一个一阶逻辑形式语言 $\mathcal{L}$, 有穷次使用以下规则得到的符号串是 $\mathcal{L}$-项：
- 个体变元是 $\mathcal{L}$-项
- 常元符号是 $\mathcal{L}$-项
- 如果 $F$ 是 $\mathcal{L}$ 中的 $n$ 元函数符号，而 $t_1, \dots, t_n$ 是 $n$ 个 $\mathcal{L}$-项，则 $F(t_1, \dots, t_n)$ 是 $\mathcal{L}$-项
{% end %}

{% admonition(type="definition", title="原子公式（atomic formula）") %}
- 如果 $R$ 是 $\mathcal{L}$ 中的 $n$ 元关系符号，而 $t_1, \dots, t_n$ 是 $n$ 个 $\mathcal{L}$-项，则 $R(t_1, \dots, t_n)$ 是原子 $\mathcal{L}$-公式
- 如果 $s, t$ 是 $\mathcal{L}$-项，则 $s \dot{=} t$ 是原子 $\mathcal{L}$-公式
{% end %}

{% admonition(type="definition", title="公式（formula）") %}
- 原子 $\mathcal{L}$-公式是 $\mathcal{L}$-公式
- 如果 $\varphi$ 和 $\psi$ 是 $\mathcal{L}$-公式，那么 $\neg \varphi, (\varphi \wedge \psi), (\varphi \vee \psi), (\varphi \to \psi)$ 是 $\mathcal{L}$-公式
- 如果 $\varphi$ 是 $\mathcal{L}$-公式且 $x$ 是个体变元，则 $\forall x \varphi$ 和 $\exists x \varphi$ 是 $\mathcal{L}$-公式
{% end %}

对 $\mathcal{L}$-公式 $\varphi$，若一部分 $\forall x \psi$, 称 $\forall x \psi$ 是量词 $\forall x$ 在这次出现的辖域（scope）。如果个体变元 $x$ 某次出现在量词的辖域中，则称其为约束出现，否则称为自由出现。至少一次自由出现的称为**自由变元**（free variable），否则称为约束变元（bounded variable）。

一个 $\mathcal{L}$-公式没有自由变元，则称之为 $\mathcal{L}$-语句（sentence）。

## 形式语义
一个 $\mathcal{L}$-结构 $\mathfrak{A}$ 包含：一个涉及所有个体的非空集合，称为**论域**（domain），并把常元符号、函数符号、关系符号解释成 $\mathfrak{A}$ 上的元素、函数、关系。

$\mathfrak{A}$ 上的一个指派（assignment）是一个从个体变元集合到论域的函数，这直观上说的是语境。

仿照之前的文章，剩下的语义定义部分是自然的，读者可自行思考。语义后承、有效式、逻辑等价定义同理。

{% admonition(type="theorem", title="合同引理") %}
$\mathfrak{A}$ 与 $\mathfrak{B}$ 是论域相同的两个 $\mathcal{L}$-结构，分别有指派 $\nu$ 与 $\mu$ 使得对 $\varphi$ 中出现的常元符号、函数符号、关系符号对应相等，自由变元满足 $\nu(x) = \mu(x)$, 则：

$$\mathfrak{A}, \nu \models \varphi \iff \mathfrak{B}, \mu \models \varphi$$
{% end %}

## 证明系统
### 自然演绎系统
除[命题逻辑](@/posts/logic_2.md)自然演绎系统的规则外，还有六条规则。

用记号 $\varphi[t/x]$ 表示将 $x$ 在 $\varphi$ 中每一次自由出现替换为 $t$ 所得的公式。

---

对 $\mathcal{L}$-公式 $\varphi$ 与 $\mathcal{L}$-项 $t$ 与个体变元 $x$, 称 $t$ 对于 $\varphi$ 中的 $x$ 代入自由，如果对 $t$ 中出现的每一个个体变元 $y$, $x$ 在 $\varphi$ 中的每一次自由出现都不在量词 $\forall y$ 或 $\exists y$ 的辖域内。

规则 $(\forall \text{E})$ 是说，如果 $t$ 对于 $\varphi$ 中的 $x$ 代入自由，且 $\begin{matrix} D \cr \forall x \varphi \end{matrix}$ 是 $\mathcal{L}$-推演，则：

$$
\frac{
	\begin{matrix} D \cr \forall x \varphi \end{matrix}
}{\varphi[t/x]} (\forall \text{E})
$$

相应地就有：

$$
\frac{
	\begin{matrix} D \cr \varphi[t/x] \end{matrix}
}{\exists x \varphi} (\exists \text{I})
$$

---

基于等词的直观有：

$$
\frac{~}{t \dot{=} t} (\dot{=} \text{I})
$$

$$
\frac{
	(s \dot{=} t) \quad
	\varphi[s/x]
}{\varphi[t/x]} (\dot{=} \text{E})
$$

实际上可以用它们证明对称性与传递性。

---

$z$ 是 $\mathcal{L}$ 的一个个体变元，$\varphi$ 是一个 $\mathcal{L}$-公式，如果 $z$ 不是前提且不在 $\varphi$ 中出现，则：

$$
\frac{
	\begin{matrix} D \cr \varphi[z/x] \end{matrix}
}{\forall x \varphi} (\forall \text{I})
$$

$z$ 是 $\mathcal{L}$ 的一个个体变元，$\varphi$ 是一个 $\mathcal{L}$-公式，如果 $z$ 不在额外的前提中出现，也不在 $\varphi$ 与 $\psi$ 中出现，则：

$$
\frac{
	\begin{matrix} ~ \cr D_1 \cr \exists x \varphi \end{matrix} \quad
	\begin{matrix} [\varphi[z/x]] \cr D_2 \cr \psi \end{matrix}
}{\psi} (\exists \text{E})
$$

---

语形后承、可证、一致的定义仿照之前文章。

### 希尔伯特式证明系统
希尔伯特式证明系统包含分离规则及以下公理（模式）（不涉及 $\exists$ 因为 $\exists x$ 与 $\neg \forall x \neg$ 一样）：

命题逻辑三个公理模式的一阶版本：

$$\forall x_1 \cdots \forall x_n (\psi \to (\varphi \to \psi))$$

$$\forall x_1 \cdots \forall x_n ((\varphi \to (\psi \to \chi)) \to ((\varphi \to \psi) \to (\varphi \to \chi)))$$

$$\forall x_1 \cdots \forall x_n ((\neg \psi \to \neg \varphi) \to (\varphi \to \psi))$$

以及（出现情况说明省略）：

$$\forall x_1 \cdots \forall x_n (t \dot{=} t)$$

$$\forall x_1 \cdots \forall x_n ((s \dot{=} t) \to (\varphi[s/x] \dot{=} \varphi[t/x]))$$

$$\forall x_1 \cdots \forall x_n (\forall x \varphi(x, x_1, \dots, x_n) \to \varphi(x, x_1, \dots, x_n)[t/x])$$

$$\forall x_1 \cdots \forall x_n (\forall x (\varphi(x, x_1, \dots, x_n) \to \psi(x, x_1, \dots, x_n)) \to (\forall x \varphi(x, x_1, \dots, x_n) \to \forall x \psi(x, x_1, \dots, x_n)))$$

$$\forall x_1 \cdots \forall x_n (\varphi(x, x_1, \dots, x_n) \to \forall x \varphi(x, x_1, \dots, x_n))$$

## 应用
### 形式化
自然语言基于一阶逻辑的形式化，我们在数学里已经比较熟悉了。

例如说，“所有猫都是哺乳动物”应形式化为：

$$\forall x (\text{Cat}(x) \to \text{Mammal}(x))$$

自然语言通过堆叠修饰语来构造复合谓词。有一些修饰语是相交性（intersectional）的，如果“红色的苹果”，有一些是包含性（subsective）的，如“高明的外科医生”，甚至有一些是掠夺性（privative）的，如“玩具熊”。

形式化中存在一些麻烦：
- 量词顺序与模式歧义
- Peter Geach 的经典驴句是 Every farmer who owns a donkey beats it. 其中驴对应变量的辖域有所错位，对此的回应是引入新的可以被保留到从句外的话语指称，使得后续代词可以拾取它，不定描述的语义被看作一种“语境更新操作”
- 内涵语境，“小明知道鲁迅写了某篇小说，但不知道周树人写了某篇小说”
- 必要条件与充分条件，对于使用模态逻辑，多数哲学家选择保留可能世界语义学语义上的好处，却拒绝其本体论

### 超越
一阶逻辑有一些无法做到的事，如无法说明某图是连通的（论域是无穷的时无法处理），可以用紧致性原理证明这一点。

二阶逻辑增加了对谓词和关系本身的量词，即：可以用 $\forall P$ 这样的量词。

自由逻辑（free logic）允许常元或项是“空”的，而 $\exists y (x = y)$ 就意为 $x$ 存在。

一阶模态逻辑在一阶逻辑中直接引入了模态算子。Saul Kripke 主张专名是严格指示词：它在每个该对象存在的可能世界中都指称同一对象。

Lindström 定理表明，一阶逻辑在紧致性和向下 Löwenheim-Skolem 性质下是表达能力最强的逻辑系统。也就是，任何严格更具表达力的逻辑，必至少损失其中一个性质，这解释了一阶逻辑的特殊地位具有某种理论必然性。但我们对这些性质的重视最终取决于我们对逻辑的目的和界限的看法，是一种哲学选择。

## 计算
### 理论
{% admonition(type="definition", title="一阶逻辑的理论") %}
理论是一组一阶逻辑句子（集合），这些句子被称为这个理论的公理。这些句子用到的非逻辑符号（常量名字，谓词，函数）被称为这个理论的初始概念。

对理论 $T$ 如果 $T \vdash \varphi$, 我们称 $\varphi$ 为 $T$ 的**定理**/逻辑后果/理论后果。
{% end %}

一个好的理论至少得是一致的，即 $x \neq x$ 不是这个理论的逻辑后果。

如果一个理论 $T$ 既不能证明 $\varphi$ 也不能证明 $\neg \varphi$ 则我们说 $\varphi$ 相对于 $T$ 独立。

Leibniz 的一个哲学理想（信件中所透露）是，当两个哲学家对是否应当接受一句话产生争议的时候，只需要找到他们共同接受的前提，然后计算这个前提是否能够推出那句话或者那句话的否。一个弱化版是形式化到一阶逻辑系统中（原本的理想是所有的谓词直接对应于现实世界的某个东西，但物理学告诉我们这还很远）。

这里的困难在于：
- （一阶）逻辑太弱了
- 共同接受的理论太弱以至于明显我们关心的公式是独立的
- 无法在如何形式化争议句子上找到共识
- 计算不保证能得到结果

### 可计算性
我们来看第四点困难。容易发现命题逻辑是可计算的。而对一阶逻辑的可计算性 Hilbert 相当乐观。

Tableau/决策树计算方法的想法是：从我们想检查的公式集开始，逐步寻找必须为真的公式。例如说，想要证明：

$$\set{p \to (q \vee r), q \to s, r \to s} \vdash p \to s$$

我们在树的根部放：

$$p \to (q \vee r), q \to s, r \to s, \neg (p \to s)$$

展开最后一个条件得到 $p, \neg s$. 写在往下延申的一个节点。

展开 $p \to (q \vee r)$ 得到两个分支 $\neg p$ 与 $q \vee r$. 第一个分支立刻与 $p$ 矛盾，将第二个分支展开得到 $q$ 与 $r$, 对此分别展开 $q \to s$ 与 $r \to s$, 所有分支都被关闭。因此原推理有效。

---

对一阶逻辑来说，考虑一元谓词 $P, Q$, 每个对象恰好满足 $P(x) \wedge Q(x), P(x) \wedge \neg Q(x), \neg P(x) \wedge Q(x), \neg P(x) \wedge \neg Q(x)$.

我们用 $E_A$ 表示 $\exists x (P(x) \wedge Q(x))$, 则量词消去的目标是：把一阶公式变为只涉及 $E_A, E_B, E_C, E_D$ 及原子命题的命题逻辑公式。可以证明，判断一个只用一元谓词的一阶逻辑公式是否有效是可计算的。

### 计算
Turing 指出了，对完全的一阶逻辑来说，是不可判定的。[^turing]

为此，我们考虑什么是计算。有以下等价的计算模型：
- 图灵机
- Church 的一般递归函数和 [λ 演算](@/posts/lambda_calculus.md)
- Emil Post 的[重写系统](@/posts/lambda_calculus.md#rewriting-systems)

计算理论非常有趣，读者可自行查阅资料。

我们考虑停机问题（输入一串描述图灵机 $T$ 的符号 $s$, 输出 $T$ 运行在 $s$ 上是否最后会停下来）。如果停机问题是可计算的，令计算停机问题的图灵机为 $H$, 此时我们可以写一个图灵机 $L$ 来执行如下任务：输入一串符号 $s$, 用 $H$ 计算 $H(s)$, 如果输出 true 则进入不停往下一个格子写 $1$ 的循环，反之停下来。我们把其代码记作 $c(L)$, 则在 $L(c(L))$ 是否停机上出现矛盾。

从而我们知道停机问题是不可计算的，对此的形式化过程略去。而为了把图灵机运行编码进一阶逻辑，考虑经典的使用纸带的状态转移机，形式化过程略去。从而说明了一阶逻辑的有效性的不可计算性。

---

[^turing]: Alan M. Turing, "On Computable Numbers, with an Application to the Entscheidungsproblem," *Proceedings of the London Mathematical Society* s2-42, no. 1 (1936): 230-265, <https://doi.org/10.1112/plms/s2-42.1.230>.

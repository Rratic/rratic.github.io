+++
title = "类型论发展与思想综述"
date = 2026-07-21

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "基石"]
+++

阅读 Trebor 的高观点小册子[《类型论简史》](https://zhuanlan.zhihu.com/p/609996873)的笔记（跳过了范畴语义部分）。

<!-- more -->

## 语法与语义
关于[**语法/语形**和**语义**](@/posts/logic_1.md)，我们已经比较熟悉了。语法最容易定义的方式是使用字符串定义，但我们其实并不关心字符串的技术细节，而是认为它只是表示语法的一种方式。如以下梗图：

![语法](/images/misc/2026_07_21.png)

作为一个例子，考虑群的语法。同寻常一样[归纳定义合法的表达式](@/posts/logic_4.md)；然后引入等词，等词是等价关系，且是合同[^congruent]关系；然后用等词给出群运算规则。

而理想的定义方式是，考虑一列自由群 $F_n (n \in \N)$，它们与它们的群同态构成一个范畴，取它的对偶 $\tau$ 称为群论的**语法范畴**。会发现从 $\tau$ 到到集合范畴的保积函子与群一一对应[^group-correspondence]，到拓扑空间范畴的保积函子与拓扑群一一对应，到光滑流形范畴的保积函子与 Lie 群一一对应。

这一观点来自于：当我们考虑群 $G$ 中满足 $x^2 y^2 = e$ 的元素时，可以看成 $\braket{x, y | x^2 y^2}$ 到 $G$ 的同态。将群看作上述保积函子，就是 **Lawvere 函子语义**。

## 简单类型论
类型论起源于 Russell 悖论，他本人走向了与 ZFC 不同的解决方案。一个由 Alonzo Church 简化的版本（及其变体）是**简单类型论**，[我们已经比较熟悉了](@/posts/haskell_1.md)。注意，元素不能独立于类型存在：我们不能先假设有一个元素，然后讨论它属于什么类型。[^similar-to-set]如果用 $o$ 表示所有命题的类型，则对类型 $s$ 可以将“任意”表述成一个 $\forall: (s \to o) \to o$，由此简单类型论会被称为**高阶逻辑**。[^higher-order-logic]

简单类型论的 **Henkin 模型**是对每个类型 $s$ 选取一个集合 $M_s$ 并要求 $M_{s_1 \to s_2} \subseteq \mathrm{Hom}(M_{s_1}, M_{s_2})$，它是可靠、完全的。标准模型进一步要求把“$\subseteq$”换成相等。

如果单独研究其中函数的形成与使用部分，就得到我们熟悉的[无类型 λ 演算](@/posts/lambda_calculus.md)，而 Moses Schönfinkel 提出了组合子逻辑，不使用变量来描述函数。不过这不能直接用于逻辑，我们知道存在[不动点组合子](@/posts/lambda_calculus.md#fixed-point) $\mathrm{fix}$，考虑 $\neg$ 就会有 $\mathrm{fix} \neg = \neg (\mathrm{fix} \neg)$，导致不一致。

这个问题在简单类型论中不会出现，观察其区别，使用[重写系统](@/posts/lambda_calculus.md#rewriting-systems)可以说明它是弱停机的，而 Sterling 考虑了更广义的 normalization 概念。我们定义组合子演算中典范形式是不含 $\mathrm{S}ABC$ 或 $\mathrm{K}AB$ 的表达式。为它赋予类型 $(\alpha \to \beta \to \gamma) \to (\alpha \to \beta) \to (\alpha \to \gamma)$ 及 $\alpha \to \beta \to \alpha$，并定义基础类型 $\mathrm{t}, \mathrm{f}: \mathbb B$，则易见典范形式只有 $\mathrm{t}, \mathrm{f}$，在 Henkin 模型中对表达式长度归纳即知：简单类型组合子演算中，表达式都有等价的典范表达式。

简单类型 λ-演算的典范化较为复杂，这里略去。其语法范畴是 $1 \rightrightarrows \mathbb B$ 生成的笛卡尔积闭范畴（Cartesian Closed Category）。

对于无类型的 λ-演算与组合子演算，寻找语义表示也是有意义的。一个重要的观察是“无类型”意味着单类型。设所有的表达式有相同的类型 $D$，则：

$$(D \to D) \cong D$$

集合论上 $D$ 如果是集合则只能是平凡的，而对于拓扑空间则大有所为，相关的研究称为论域论。

## Curry–Howard 对应
### 对应
Curry 在 1934 年初步注意到组合子演算与 [Hilbert 公理系统](@/posts/logic_2.md)的对应关系，这之后被逐步推广，得到类型与命题的许多相似性，也就是有名的 **Curry–Howard 对应**。我们不妨将相似性升格为同一性：

<p style="text-align: center">类型是命题</p>

有了命题逻辑的对应后，我们可以参考着考虑谓词逻辑的对应。$\forall x: P(x)$ 对应的是依值函数，$\exists x: P(x)$ 对应的是依值对，扩展的对应可参见[之前笔记](@/posts/type_theory_1.md)。

一个经典的例子是 F 系统，它在简单类型 λ-演算之外添加了形如 $\forall X. \beta$ 的类型，其中 $X$ 是类型变量，$\beta$ 是可以含 $X$ 的表达式，其元素 $\Lambda X. M$ 满足对任意类型 $\alpha$ 有 $M[X/\alpha]$ 是 $\beta[X/\alpha]$ 的元素。例如：

$$(\Lambda X. \lambda x^X.\ x): (\forall X. X \to X)$$

Girard 证明了（移除排中律的）二阶逻辑中可以证明是全函数[^total-function]的递归函数在 F 系统中都能定义，Reynolds 则给出了一个反向的对应，这被称为 **Girard–Reynolds 同构**。

纯类型系统与 λ-立方我们略去不谈。

### 构造主义
Curry–Howard 对应的不完美之处在于排中律（Law of Excluded Middle (LEM)）。对于简单类型 λ-演算，可以观察语义考虑这样的模型：命题是开集，全集是真命题，空集是假命题，用并集与交集表示 $\vee$ 与 $\wedge$，将 $\neg p$ 看成与 $p$ 矛盾的命题中最弱的 $(X \setminus U)^\circ$，则排中律是 $X = U \cup (X \setminus U)^\circ$，这不一定成立。或者分析语法，没有条件的情况下 $p \vee q$ 必须确定地构造出哪一边是成立的，而对于 $p \vee \neg p$ 我们无法给出。

Hilbert 之前的大部分数学研究可以完全绕过排中律，而之后的数学也可以发展许多无需排中律的版本。我们认为排中律使得大量原先不等价的命题变得等价，因此可以说，

<p style="text-align: center">没有排中律使数学变丰富了</p>

之前的分析语法暗示了，任何数学对象都必须构造出来才可以认为存在。这类数学哲学思想称为**构造主义**。[^bhk]

19 世纪，许多数学定义被严格化，如实数作为有理数的 Cauchy 列、Cantor 的工作、Frege 将自然数 $n$ 定义为所有恰好有 $n$ 个元素的集合构成的集合。Kronecker 与 Poincaré 等在 19 世纪末对此质疑，认为使用无穷集合并且常常是非构造性质的概念不能作为数学的根基。

Brouwer 继承 Kronecker 思想，对 [Hilbert 纲领](@/posts/logic_4.md)提出抨击，认为数学思想先于语言而存在，这样的数学哲学观点称为**直觉主义**。Brouwer 认为，只有对于有限个事物的命题才能使用排中律。但 Brouwer 的直觉主义中并不严格要求一切数学对象都被完全确定地构造出来，允许了“自由选择序列”，Brouwer 用此构造出了直觉主义的实数集。[^intuitionistic-real]Heyting 提出了一套刻画直觉主义的形式逻辑系统，大致就是一阶逻辑去掉排中律，因此去掉排中律的逻辑会被不当地称为用直觉主义逻辑，正确的名称是**中性逻辑**。

Markov 构建了**递归构造主义**/俄罗斯构造主义，提出所有的数学对象都使用适当的递归函数（可计算函数）构造。其中构造的实数即可计算实数，只有可数个。

Bishop 则成功展示了数学的许多重要部分都能在构造主义中发展。Bishop 认为，要构造一个集合，只需要说明如何构造它的元素，并且说明构造出的两个元素如何判断相等。这种定义集合的方式现在被称为 Bishop 集合，在类型论
下看这就是一个类型上配备了一个等价关系，称作**广集**（setoid）。对于 $(A, \sim_A), (B, \sim_B)$ 我们可以规定 $A \implies B$ 是在 $A \to B$ 上配备等价关系：

$$(f \sim_{A \implies B} g) \iff \forall x: f(x) \sim_B g(x)$$

1970 年代，Martin-Löf 提出了[**构造主义类型论**](@/posts/type_theory_1.md)（Martin-Löf Type Theory (MLTT)）。

### 引入排中律
我们考虑排中律能否有加入 Curry–Howard 对应而不是强行作为公理的方法。排中律等价于 Peirce 定律，这对应到**计算续体**（call-with-current-continuation (call/cc)）。其类型为 $((\alpha \to \beta) \to \alpha) \to \alpha$，作用是实现捕获续体，即 $u(\text{call/cc}(f)) = u(f(u))$.

尽管这确实是一种类型论的解释，它破坏了确定性。其它引入方式同样会破坏一些好的性质，因为我们知道停机问题无法判定，因此要么类型论中限制排中律（从而无法证明要么停机要么不停机），要么弱化可计算性。

## Martin-Löf 类型论
此类型论的详细说明已在之前笔记中展示，关于**归纳类型**及更强大的变体这里略去。

### 相等类型
相等在 Curry–Howard 对应下是对 $x, y: A$ 有**相等类型**，写作 $x =_A y$，当它被实现时称 $x, y$ **命题相等**。而等词级别的相等是**判值相等/定义相等**。

类型论必须要有判值相等。这可以是平凡的，如只有字面相等的表达式才判值相等，此时称为**弱类型论**。或者可以设置规则让命题相等推出判值相等，得到**外延类型论**（extensional type theory）。一般讨论的 Martin-Löf 类型论是内涵类型论（intensional type theory）。

相等类型的构造子是 $\mathrm{refl}_a: a =_A a$，而对应的消去子是：

$$\mathrm{J}: \prod _{P: \prod _{a, b: A} a = b \to \mathcal{U}} \left[\prod _{c: A} P\ c\ c\ \mathrm{refl} _c\right] \to \prod _{a, b: A} \prod _{p: a = b} P\ a\ b\ p$$

这说的是，如果命题在自反情况下能被证明，那么在给予 $a =_A b$ 证明时在 $(a, b)$ 情况下也能被证明。这可以[证明](@/posts/agda_1.md)对称性、传递性、替换等，但不能证明函数的外延性（逐点相等推出相等）。

Martin-Löf 类型论的一个缺憾是，很难加入**商类型**，而让类型论仍具有好的性质。

### 宇宙
在 Martin-Löf 类型论中，如果 $A: \mathcal{U} _i, B: \mathcal{U} _j$，那么就有 $A \to B: \mathcal{U} _{\max \set{i, j}}$，这使得它成为一种**直谓类型论**（predicative type theory）[^predicative]。这会带来关于应使用何宇宙层级的技术麻烦。

**归纳构造演算**（Calculus of Inductive Constructions (CIC)）允许最底层的宇宙有非直谓性，即 $j = 1$ 时 $A \to B: \mathcal{U}_1$.

由于对宇宙的直谓性安排，容易证明 Martin-Löf 类型论有自洽性（不存在（没有自由变量）的 $u: \mathbf{0}$）。使用复杂的方法还可以证明 Martin-Löf 类型论有典范性。

## 同伦类型论
### K 原理
回顾 J 原理，我们对于相等类型有一个大致的直觉是 $a = b$ 只有 $\mathrm{refl}$ 一个构造器，也就是如下 K 原理：

$$\prod_{a, b: A} \prod_{p, q: a = b} p = q$$

用 J 原理可以证明每个类型上有一个群胚（groupoid）[^groupoid]结构。我们可以构造一个将每个类型解释成群胚的模型，特别地，将相等类型 $x = y$ 解释为 $\mathrm{Hom}(x, y)$ 上的离散群胚，可以验证模型满足 J 原理，而 K 原理对应于“所有类型的群胚都是离散群胚”，不成立，因此 J 原理无法证明 K 原理。

### 群胚
从之前的模型中我们注意到有高一层的 K 原理：

$$\prod_{a, b: A} \prod_{p, q: a = b} \prod_{\alpha, \beta: p = q} \alpha = \beta$$

同样有低一层的 K 原理，不妨记作 $K_{-1}: \prod_{a, b: A} a = b$. 设原来的 K 原理是 $K_0$，可以有任意高的 K 原理 $K_n$. 可以想象如果构造一个 $2$-群胚可以证明 $K_1$ 不可证明，同理引出了 $\infty$-群胚。

对一个拓扑空间，我们取所有道路而不商去同伦，则并不严格构成群胚。让 $2$-道路是[定端同伦](@/posts/geometry_2_final.md)，$3$-道路是同伦之间的同伦，以此类推，则所有这些东西构成一个（弱）$\infty$-群胚。这也可以看成奇异复形。

由此，我们新的理解是：

<p style="text-align: center">类型是空间</p>

特别地，命题应该是其中那些满足 $K_{-1}$ 原理的类型。我们可以做定义：称类型 $A$ 为命题当且仅当可以证明：

$$\prod_{x, y: A} x = y$$

而满足 $K_0$ 原理的应该被称为集合，满足 $K_1$ 原理的应该被称为群胚，以此类推。我们现在可以把类型排中律改为**命题排中律**：

$$\prod_{A: \mathcal{U}} \mathrm{isProp}(A) \to (A + \neg A)$$

### 泛等公理
我们可以定义函数 $f: A \to B$ 是**等价**，对应于视作空间时的同伦等价。对每个 $f$，$\mathrm{isEquiv}(f)$ 是命题。我们定义 $X \simeq Y$ 为 $\sum_{f: X \to Y} \mathrm{isEquiv}(f)$. 显然有 $(X = Y) \to (X \simeq Y)$，Voevodsky 的**泛等公理**说的是，这个函数本身是一个等价，立即的推论是：

$$(X = Y) \simeq (X \simeq Y)$$

这可以给出许多推论，如等价的命题都相等、逐点相等的函数相等、同构的群相等（这与**数学结构主义**相合）。另外这可以推出类型排中律不成立，而仍与命题排中律相合。

### 高阶归纳类型
我们可以对归纳类型作推广，考虑圆类型：

```agda
data S¹ : Type where
  base : S¹          -- 点构造子
  loop : base ≡ base -- 路径构造子
```

这个定义看起来和圆的 CW 复形定义是一样的，也符合人类直觉。[The HoTT Game](https://thehottgameguide.readthedocs.io/en/latest/index.html) 演示了利用泛等公理证明其基本群是 $\Z$ 的过程。

商类型也可以作为高阶归纳类型的特殊情况存在。Guillaume Brunerie 在类型论中证明了 $\pi_4(\mathbb S^3) = \Z/2\Z$，使得类型论变成了可以研究同伦论的工具。

我们把包含泛等公理与一定量的高阶归纳类型的类型论统称为**同伦类型论**。

![覆盖](/images/misc/2026_07_22.gif)

## 立方类型论
泛等公理有很多好的结论，但作为强行加入的公理会破坏好的性质。2013 年 Marc Bezem，Thierry Coquand 与 Simon Huber 提出用立方集合构建同伦类型论的模型，其启发的类型论是**立方类型论**。立方类型论的典范性被 Sterling 等人证明。

立方类型论引入了类型 $\mathbb I$ 表示同伦论的区间，它具有端点 $0, 1$，用 $i: \mathbb I$ 表示区间上的点。对于道路 $p: x =_A y$ 我们允许取出其上某一点 $p(i): A$.

考虑用它证明 $x = y$ 推出 $f(x) = f(y)$. 取 $p: x = y$ 我们只需要定义 $q: \mathbb I \to B$ 是 $q(i) = f(p(i))$，就有 $q(0) = f(x), q(1) = f(y)$.

类似地，对两个函数逐点相等，取出 $p: \prod_{x: A} f(x) =_B g(x)$，我们只需要定义 $q: \mathbb I \to (A \to B)$ 是 $q(i) = \lambda x.\ p(x)(i)$ 即可。

---

[^congruent]: 把表达式的一部分看成相等的另一个表达式，则新表达式等于原来的表达式。
[^group-correspondence]: 我们知道空积是终对象，因此保积函子将 $F_0$ 映到某个 $\set{e}$，对应的是单位元。设 $F_1$ 映到集合 $G$，依 $F_1 \to F_0$ 的像有 $e \in G$. 又 $F_1 \to F_1$ 及 $F_2 \to F_1$ 的像给出取逆和二元运算。通过 $\tau$ 中 $F_n \times F_m = F_{n+m}$ 知确实是一一对应。
[^similar-to-set]: 如同在 ZFC 中一切可讨论的东西都是集合。
[^higher-order-logic]: 这是相对于一阶逻辑说的。二阶逻辑可以讨论关于谓词的谓词，同理可得。
[^total-function]: 在每一处有定义的函数，相对的概念是偏函数（partial function）。
[^bhk]: 关于这在逻辑上的体现可参考 [Brouwer-Heyting-Kolmogorov 释义](https://ncatlab.org/nlab/show/BHK+interpretation)。
[^intuitionistic-real]: 因为不能证明实数一定成立 $(x < 0) \vee (x = 0) \vee (x > 0)$，Brouwer 采取的是分离性概念 $x \\# y \coloneqq \exists \varepsilon > 0: |x - y| > \varepsilon$，回忆这里需要具体构造出 $\varepsilon$. Brouwer 进一步提出了一些公理，由此出发会得到一些矛盾于经典逻辑的结论，如任何 $f: [0, 1] \to [0, 1]$ 都是一致连续函数。
[^predicative]: “直谓”理解为，一个量词所界定的对象不能包含该量词自身所涵盖的全部范围。在这里即不再有所有类型，而是有开放的宇宙层级。
[^groupoid]: 乘法运算是 $\ast: \mathrm{Hom}(y, z) \times \mathrm{Hom}(x, y) \to \mathrm{Hom}(x, z)$，其余同理。未指定时默认乘法运算是复合，此时即所有态射可逆的范畴。

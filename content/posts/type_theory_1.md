+++
title = "类型论导引与 Martin-Löf 类型论"
date = 2026-03-09
updated = 2026-07-30

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "基石"]
+++

发现到时候不一定要读数院的研，考虑可以做逻辑学或者是 AI for math. 仔细阅读本书 [Homotopy Type Theory: Univalent Foundations of Mathematics](https://homotopytypetheory.org/book/). 这是上学期计概老师推荐的，但当时看得太粗略了。本文包含该书第一章内容。

<!-- more -->

假定读者有简单类型论的基础（对象由类型（type）“分类”；项（term）$a$ 有类型 $A$ 记作 $a: A$）。

## 概述
### 推演系统
我们需要先提一点关于推演系统（deductive system）的事情。不正式地说，推演系统是用于推导出**判断** （judgments）的一套规则（rules）。从逻辑观点看，判断可以被认为是*外在的*存在于*元理论*中的陈述。

在一阶逻辑（可以建立集合论）中只有一种判断：一个命题有证明。一个一阶逻辑的规则实际上是一种*证明构造*的规则。而类型论的基本判断是 $a: A$. 当 $A$ 表达的是命题时，$a$ 被称作一个 $A$ 可证性的见证（witness）或 $A$ 真的证据（evidence）。

尽管 $a: A$ 与集合上说 $a \in A$ 有一些相似，前者是判断而后者是证明。我们不能说如果 $a: A$ 就没有 $b: B$，也不能*证否*判断 $a: A$，因为对象与它的类型是不可分割的。

类型论中处理等同的方式也与集合论不同。对 $a, b: A$ 如果类型 $a =_A b$ 被居留（inhabited）就说它们**命题相等**。与此同时我们也需要关于相等的判断，称为**判值相等/定义相等**，写作 $a \equiv b : A$ 或者简写 $a \equiv b$. 例如说对 $f: \N \to \N, f(x) = x^2$ 有 $f(3) \equiv 3^2$ by definition. 这样的相等只是从定义延申出来的，尽管技术上需要一种外在的算法来判断。

我们现在使用符号 $:\equiv$ 来引入定义相等，如上例写成 $f(x) :\equiv x^2$. 这并不会产生符号上的歧义。

判断可能会基于形如 $x: A$ 的假设（assumptions）。一组假设构成*有序列表*的称为**语境/上下文**（context），一般记作 $\Gamma$. 遵循[逻辑学](@/posts/logic_1.md)中的符号用法，用 $\Gamma \vdash \varphi$ 表示语境下推演出判断，如：

$$x: A, f: A \to B \vdash f(x): B$$

如果假设 $x: A$ 的类型 $A$ 是一个命题，那么这相当于是一个 hypothesis.

一个假设可以假定一个命题相等，但是不能假定判值相等，因为 $x \equiv y$ 并不是一个类型，并不可以有元素。有时对于一个变量 $x: A$ 我们可以替换为另一个具体的对象 $a: A$，用词说“我们不妨设 $x \equiv a$”，这并不是假设。

类似地我们也不能证明一个判值相等。有时我们说“存在一个 $f: A \to B$ 使得 $f(x) \equiv y$”，应该理解成两个分离的判断，一个是 $f: A \to B$, 另一个是 $f(x) \equiv y$.

总之，现在 Martin-Löf 类型论的判断有三种：
- $\Gamma \text{ context}$，表示 $\Gamma$ 是合法的语境
- 形如 $a: A$ 的判断
- 判断相等 $a \equiv b : A$

对推演我们有两类重要的规则。代换（substitution）规则是说，如果在语境 $\Gamma$ 下的的表达式中的自由变量能够被全部代换成语境 $\Delta$ 下的表达式，那么代换后的就是语境 $\Delta$ 下合法的表达式。弱化（weakening）规则是说，在某个语境 $\Delta$ 下的的表达式，仍然是将 $\Delta$ 做扩充之后得到的语境下的表达式。这两类规则并不需要明确假设，因为可以对所有可能的推导归纳来得出。

### 新类型的引入
在规定一个类型时，我们需要规定如何形成这个类型、如何构造与使用这个类型的元素等规则。这些包括：
1. 形成规则（formation rules）
2. 构造器/引入规则（constructors/introduction rules）
3. 消去子/消去规则（eliminators/elimination rules）
4. 计算规则，表达消去子如何作用于构造器

此外有一个可选的 uniqueness principle, 用于表达 uniqueness of maps into or out of that type, 例如判值相等 $f \equiv \lambda x.\ f(x)$; 有时并没有判值相等层面的这个规则，但可以由其它规则推出命题相等，此时称为 propositional uniqueness principle.

作为例子，我们回顾简单类型 λ 演算。

形成规则不必多言；函数类型使用 λ-表达式构造，构造规则写为：

$$\frac{\Gamma, x: A \vdash M: B}{\Gamma \vdash \lambda (x: A). M : A \to B} \text{Lam}$$

有时我们可以阅读上下文推断出 $x: A$，故可省略为 $\lambda x. M$ 或者使用方便的写法 $f(x) :\equiv M$.

函数应用的规则，即函数类型的消去规则写为：

$$\frac{\Gamma \vdash M: A \to B \quad \Gamma \vdash N: A}{\Gamma \vdash M N: B} \text{App}$$

本文中之后使用一般数学的写法 $f(x)$ 表示函数应用。我们有以下计算规则（省略语境；用 $\mapsto$ 表示[替换](@/posts/lambda_calculus.md#substitution)）：

$$\frac{x: A \vdash M: B \quad N: A}{(\lambda x. M) N \equiv M[x \mapsto N]} \beta$$

我们额外引入一个规则描述函数的外延性：

$$\frac{f: A \to B}{f \equiv \lambda x. f x} \eta$$

## 宇宙与类
在朴素集合论中并不能有一个包含所有类型，包括它自己的类型 $\mathcal{U}_\infty$. 这会导致悖论。[^COQ92]为此我们引入一组宇宙的层次：

$$\mathcal{U}_0: \mathcal{U}_1: \mathcal{U}_2: \cdots$$

这些宇宙是累积的（cumulative），即 $\mathcal{U}_i$ 的元素也是 $\mathcal{U} _{i+1}$ 的元素。不过确会引入一些麻烦。

上述两条写成规则是：

$$\frac{\Gamma \text{ context}}{\Gamma \vdash \mathcal{U} _i: \mathcal{U} _{i+1}} \mathcal{U}\text{-Intro}$$

$$\frac{\Gamma \vdash A: \mathcal{U} _i}{\Gamma \vdash A: \mathcal{U} _{i+1}} \mathcal{U}\text{-Cumul}$$

我们说 $A$ 是一个类型，如果它在某个宇宙 $\mathcal{U}_i$ 中。我们通常想要避免特别指出 $i$ 是什么，假设可以正确给出层级，写作 $A: \mathcal{U}$. 这种写法被称作 typical ambiguity. 当我们指定某个宇宙 $\mathcal{U}$ 时，称它含有的类型是小类型（small types）。为了建模一组关于类型 $A$ 变动的类型，我们使用 $B: A \to \mathcal{U}$. 这些函数称为类型类（families of types）或依值类型（dependent types）。一个例子是 $\mathrm{Fin}: \N \to \mathcal{U}$ 其中 $\mathrm{Fin}(n)$ 是一个恰有 $n$ 个元素的类型。一个平凡的例子是 $(\lambda (x: A).\ B): A \to \mathcal{U}$. 而反例是并没有类型类 $\lambda (i: \N).\ \mathcal{U}_i$.

## 依值
### 依值函数类型
类型论中我们使用函数类型的更普遍版本：依值函数类型（Π-type/dependent function type）。依值函数的陪域类型可以随输入的值变动。对 $A: \mathcal{U}$ 及类 $B: A \to \mathcal{U}$ 我们将对应的依值函数类型写作：

$$\prod_{(x: A)} B(x) \text{\quad or \quad} \Pi (x: A), B(x) \text{\quad or \quad} (x: A) \to B(x)$$

我们列出其形成、构造、消去、计算、唯一性规则：

$$\frac{\Gamma \vdash A: \mathcal{U} _i \quad \Gamma, x: A \vdash B: \mathcal{U} _i}{\Gamma \vdash \prod _{(x: A)} B: \mathcal{U} _i} \Pi\text{-Form}$$

$$\frac{\Gamma, x: A \vdash b: B}{\Gamma \vdash \lambda (x: A).b: \prod_{(x: A)} B} \Pi\text{-Intro}$$

$$\frac{\Gamma \vdash f: \prod_{(x: A)} B \quad \Gamma \vdash a: A}{\Gamma \vdash f(a): B[a/x]} \Pi\text{-Elim}$$

$$\frac{\Gamma, x: A \vdash b: B \quad \Gamma \vdash a: A}{\Gamma \vdash (\lambda (x: A). b)(a) \equiv b[a/x]: B[a/x]} \Pi\text{-Comp}$$

$$\frac{\Gamma \vdash f: \prod_{(x: A)} B}{\Gamma \vdash f \equiv (\lambda x. f(x))} \Pi\text{-Uniq}$$

引入规则有对应的判断相等规则：

$$
\frac{
	\Gamma \vdash A: \mathcal{U} _i \quad
	\Gamma, x: A \vdash B: \mathcal{U} _i \quad
	\Gamma, x: A \vdash b \equiv b': B
}{\Gamma \vdash \lambda x. b \equiv \lambda x. b'} \Pi\text{-Intro-Eq}
$$

同样地，构造规则与消去规则也有关于判断相等的规则，这里略去。

容易发现，$x$ 不在 $B$ 中自由出现时，实际上有 $A \to B :\equiv \prod_{(x: A)} B$，我们将它采取为普通函数类型的定义。

现在我们可以定义多态的（polymorphic）函数。这是说先传入一个参数表示类型，然后作用于这个类型的元素。例如：

$$\mathrm{id}: \prod_{(A: \mathcal{U})} A \to A$$

$$\mathrm{id} :\equiv \lambda (A: \mathcal{U}).\ \lambda (x: A).\ x$$

定义也可简写为 $\mathrm{id}_A(x) :\equiv x$，由于可以阅读上下文推断类型，直接写为 $\mathrm{id}(x)$ 是无歧义的。

一个更不那么平凡的例子是：

$$\mathrm{swap}: \prod_{(A: \mathcal{U})} \prod_{(B: \mathcal{U})} \prod_{(C: \mathcal{U})} (A \to B \to C) \to (B \to A \to C)$$

$$\mathrm{swap}_{A, B, C}(g)(b, a) :\equiv g(a, b)$$

### 积类型
对类型 $A, B: \mathcal{U}$ 我们引入类型 $A \times B: \mathcal{U}$，其元素为满足 $a: A, b: B$ 的 $(a, b)$，不同于集合论，有序对在类型论是和函数一样的基础概念。

所以我们如何*使用*有序对呢？对于 $f: A \times B \to C$, 我们可以发展出一个消去规则：对每个 $g: A \to B \to C$ 可以定义出一个函数 $f: A \times B \to C$ 为 $f((a, b)) :\equiv g(a)(b)$.

也就是说，在类型论中，一个 $A \times B$ 上的函数在我们确定了其在有序对处的值时立即变得良定义，以此（更准确地说是从它的更一般的版本）我们能够*证明* $A \times B$ 的元素是有序对。从范畴论视角看我们可以说 $(-) \times B$ 是 $B \to (-)$ 的左伴随（left adjoint）。

我们可以从这个消去规则推导出投射函数 $\mathrm{pr}_1((a, b)) :\equiv a$ 与 $\mathrm{pr}_2((a, b)) :\equiv b$.

为了避免每次都使用这个规则，我们可以定义一个函数：

$$\mathrm{rec}_{A \times B}: \prod _{C: \mathcal{U}} (A \to B \to C) \to A \times B \to C$$

$$\mathrm{rec}_{A \times B}(C, g, (a, b)) :\equiv g(a)(b)$$

之后就可以像 $\mathrm{pr}_1 :\equiv \mathrm{rec} _{A \times B}(A, \lambda a.\ \lambda b.\ a)$ 一样使用。

我们称 $\mathrm{rec}_{A \times B}$ 是积类型的递归函数（recursor）. 这里没有明显体现递归，因为积类型是归纳类型的一个退化例子。

为了能够在积类型上定义依值函数，我们必须把递归函数一般化。可以从下式中通过 $f((x, y)) :\equiv g(x)(y)$ 定义出一个 $f: \prod_{(x: A \times B)} C(x)$:

$$g: \prod_{(x: A)} \prod_{(y: B)} C((x, y))$$

由此我们可以构建一个函数：

$$\mathrm{uniq}_{A \times B}: \prod _{x: A \times B} ((\mathrm{pr}_1(x), \mathrm{pr}_2(x))) = _{A \times B} x$$

关于相等类型具体的内容会在之后的节中介绍。现在我们只需知道对 $x: A$ 有 $\mathrm{refl}_x: x=_A x$. 由此可以定义：

$$\mathrm{uniq}_{A \times B}((a, b)) :\equiv \mathrm{refl} _{(a, b)}$$

这是因为存在判值相等 $(\mathrm{pr}_1((a, b)), \mathrm{pr}_2((a, b))) \equiv (a, b)$.

更进一步可以有归纳（induction）函数：

$$\mathrm{ind}_{A \times B}: \prod _{C: A \times B \to \mathcal{U}} \left(\prod _{(x: A)} \prod _{(y: B)} C((x, y))\right) \to \prod _{x: A \times B} C(x)$$

可见递归只是归纳的一个特例。我们也称归纳为（依值）消去子（ (dependent) eliminator），递归为不依值消去子（non-dependent eliminator）。

我们另外引入空积类型（nullary product type）或曰单位类型（unit type）$\mathbf{1}: \mathcal{U}$，其唯一元素 $\star: \mathbf{1}$. 关于它的讨论从略。

### 依值对类型
我们扩展积类型到依值对类型（Σ-type/dependent pair type）。这对应于集合论中一个指标集对应一组集合的无交并。给定类型 $A: \mathcal{U}$ 与类 $B: A \to \mathcal{U}$, 写作：

$$\sum_{(x: A)} B(x) \text{\quad or \quad} \Sigma (x: A), B(x) \text{\quad or \quad} (x: A) \times B(x)$$

其构造方法也是通过对 $a: A$ 与 $b: B(a)$ 配对 $(a, b)$.

我们写出第一个的投射函数为：

$$\mathrm{pr}_1: \left(\sum _{(x: A)} B(x)\right) \to A$$

$$\mathrm{pr}_1((a, b)) :\equiv a$$

第二个投射函数需要是一个依值函数，类型为：

$$\mathrm{pr}_2: \prod _{p: \sum _{x: A} B(x)} B(\mathrm{pr}_1(p))$$

为此我们需要一个用于 Σ-type 的归纳原则，也即希望从 $C: (\sum_{x: A} B(x)) \to \mathcal{U}$ 及：

$$g: \prod_{a: A} \prod_{b: B(a)} C((a, b))$$

推导出：

$$f: \prod _{p: \sum _{x: A} B(x)} C(p)$$

由于与积类型的情况类似，此处省略递归与归纳函数的内容。

作为一个有趣的例子，对类型 $A, B$ 及 $R: A \to B \to \mathcal{U}$ 考察：

$$\mathrm{ac}: \left(\prod_{(x: A)} \sum_{(y: B)} R(x, y)\right) \to \left(\sum_{(f: A \to B)} \prod_{(x: A)} R(x, f(x))\right)$$

$$\mathrm{ac}(g) :\equiv \left(\lambda x.\ \mathrm{pr}_1(g(x)), \lambda x.\ \mathrm{pr}_2(g(x))\right)$$

如果把 $\prod$ 读作“对任意”，把 $\sum$ 读作“存在”，那么这个类型表示：如果对任意 $x: A$ 存在一个 $y: B$ 满足 $R(x, y)$, 那么存在一个函数 $f: A \to B$ 使得对任意 $x: A$ 有 $R(x, f(x))$. 由于这和集合论中的选择公理比较像，这个函数传统上称为类型论版的选择公理。

依值对类型通常用于表达一些有多片数据的数学结构。例如一个原群 magma 是 $(A, m)$, 其中 $A: \mathcal{U}$ 及 $m: A \to A \to A$. 因此说：

$$\mathrm{Magma} :\equiv \sum_{A: \mathcal{U}} (A \to A \to A)$$

最后，为了方便起见，我们使用符号 $(x, y, z) :\equiv (x, (y, z))$ 等等。

### 余积类型
对 $A, B: \mathcal{U}$ 可以有余积类型（coproduct type）$A + B: \mathcal{U}$ 对应于集合论中的无交并。我们也引入一个空版本：空类型（empty type）$\mathbf{0}: \mathcal{U}$.

$A + B$ 的元素有左、右嵌入（injection）两种构造方法:对 $a: A$ 的 $\mathrm{inl}(a): A + B$ 与对 $b: B$ 的 $\mathrm{inr}(b): A + B$.

为了构造非依值函数 $f: A + B \to C$，需要函数 $g_0: A \to C$ 与 $g_1: B \to C$, 然后写：

$$
\begin{matrix}
	f(\mathrm{inl}(a)) :\equiv g_0(a) \cr
	f(\mathrm{inr}(b)) :\equiv g_1(b)
\end{matrix}
$$

这种定义称为 case analysis. 如之前一样我们可以推导出递归和归纳函数。

特别地 $\mathbf{0}$ 的递归函数是 $\mathrm{rec}_\mathbf{0}: \prod _{C: \mathcal{U}} \mathbf{0} \to C$，这对应于经典的[爆炸律](@/posts/logic_2.md#Remark)。

### 布尔类型
布尔类型（boolean）希望形如 $0_{\mathbf{2}}, 1_{\mathbf{2}}: \mathbf{2}$ 的结构。它可以由 $\mathbf{1} + \mathbf{1}$ 给出，但为方便起见给出特别的讨论。

递归函数对应于 if-then-else 结构：

$$\mathrm{rec} _{\mathbf{2}}: \prod _{C: \mathcal{U}} C \to C \to \mathbf{2} \to C$$

$$
\begin{matrix}
	\mathrm{rec} _{\mathbf{2}}(C, c_0, c_1, 0 _{\mathbf{2}}) :\equiv c_0 \cr
	\mathrm{rec} _{\mathbf{2}}(C, c_0, c_1, 1 _{\mathbf{2}}) :\equiv c_1
\end{matrix}
$$

把它打包成归纳原则如下：

$$\mathrm{ind}_{\mathbf{2}}: \prod _{(C: \mathbf{2} \to \mathcal{U})} C(0 _{\mathbf{2}}) \to C(1 _{\mathbf{2}}) \to \prod _{x: \mathbf{2}} C(x)$$

通过传入 $\mathrm{inl}(\mathrm{refl} _{0 _\mathbf{2}})$ 与 $\mathrm{inr}(\mathrm{refl} _{1 _\mathbf{2}})$ 可以构造出一个：

$$\prod_{x: \mathbf{2}} (x = 0_\mathbf{2}) + (x = 1_\mathbf{2})$$

布尔类型也可以用于定义二元的无交并（同集合论中所用的方法）：

$$A + B :\equiv \sum_{x: \mathbf{2}} \mathrm{rec}_{\mathbf{2}}(\mathcal{U}, A, B, x)$$

也可定义笛卡尔积：

$$A \times B :\equiv \prod_{x: \mathbf{2}} \mathrm{rec}_{\mathbf{2}}(\mathcal{U}, A, B, x)$$

我们常将 $0_{\mathbf{2}}, 1_{\mathbf{2}}$ 称作 `false` 与 `true`. 但我们不把 $\mathbf{2}$ 的元素视作真值或者命题。

### 自然数
自然数 $\N$ 的元素是通过 $0: \N$ 与 $\mathrm{succ}: \N \to \N$ 定义的。现在其上的递归与归纳函数更符合常识上的“递归”、“归纳”含义了。

使用素材 $c_0: C$ 与 $c_s: \N \to C \to C$ 可以定义：

$$
\begin{matrix}
	f(0) :\equiv c_0 \cr
	f(\mathrm{succ}(n)) :\equiv c_s(n, f(n))
\end{matrix}
$$

这称为通过原始递归（primitive recursion）定义。通过原始递归定义的*显然*是*可计算的（computable）*。

把原始递归打包成递归函数：

$$\mathrm{rec}_{\N}: \prod _{C: \mathcal{U}} C \to (\N \to C \to C) \to \N \to C$$

同样一般化为归纳法：

$$\mathrm{ind}_{\N}: \prod _{(C: \N \to \mathcal{U})} C(0) \to \left(\prod _{(n: \N)} C(n) \to C(\mathrm{succ}(n))\right) \to \prod _{(n: \N)} C(n)$$

一个自然数的性质（property）是由类型类 $P: \N \to \mathcal{U}$ 表达的。由此，上式即是数学归纳法。

## 模式匹配与递归
我们有时会这样定义函数：

$$
\begin{matrix}
	f(\mathrm{inl}(a)) :\equiv \Phi_0 \cr
	f(\mathrm{inr}(b)) :\equiv \Phi_1
\end{matrix}
$$

这本质上是使用了递归函数：

$$f :\equiv \mathrm{rec}_{A+B}(C, \lambda a.\ \Phi_0, \lambda b.\ \Phi_1)$$

而对：

$$
\begin{matrix}
	f(0) :\equiv \Phi_0 \cr
	f(\mathrm{succ}(n)) :\equiv \Phi_s
\end{matrix}
$$

则本质上是：

$$f :\equiv \mathrm{rec}_{\N}(C, \Phi_0, \lambda n.\ \lambda r.\ \Phi_s')$$

这种使用递归/归纳函数的方法非常方便，且在具体实现中作为语法糖存在，称为模式匹配（pattern matching）。当然它是有局限的，例如说 $f(\mathrm{succ}(n)) :\equiv$ 的右侧只能出现 $f(n)$ 而不能出现 $f(\mathrm{succ}(\mathrm{succ}(n)))$ 这样的东西。

## 命题作为类型
基于基本的观测有这样的对应：

| 逻辑 | 类型论 |
| :--: | :---: |
| $\top$ | $\mathbf{1}$ |
| $\bot$ | $\mathbf{0}$ |
| $A \wedge B$ | $A \times B$ |
| $A \vee B$ | $A + B$ |
| $A \implies B$ | $A \to B$ |
| $\neg A$ | $A \to \mathbf{0}$ |
| $\exists_{x: A} B(x)$ | $\sum_{(x: A)} B(x)$ |
| $\forall_{x: A} B(x)$ | $\prod_{(x: A)} B(x)$ |

我们把 $\mathbf{0}$ 的实现称为一个矛盾（contradiction）。并不存在证明一个矛盾的方法。[^contradiction]

现在考虑实现 $(A \to \mathbf{0}) \times (B \to \mathbf{0}) \to (A + B \to \mathbf{0})$. 我们可以开“洞”然后整理：

$$
\begin{matrix}
	f((x, y))(\mathrm{inl}(a)) :\equiv \Box : 0 \cr
	f((x, y))(\mathrm{inr}(b)) :\equiv \Box : 0
\end{matrix}
$$

此时就可以看出两个洞应该填入 $x(a)$ 与 $y(b)$.

现在我们无法构造 $((A \to \mathbf{0}) \to \mathbf{0}) \to A$ 的实例，因为在直觉主义逻辑中并不成立它（双重否定消除）。直觉主义中并不存在经典逻辑的排中律或者通过矛盾来证明。需要额外引入公理或者 call/cc. 反过来的双重否定引入则是可以构造实例的。

现在考虑一个带谓词的逻辑对应的例子：

$$\left(\prod_{(x: A)} P(x) \times Q(x)\right) \to \left(\prod_{(x: A)} P(x)\right) \times \left(\prod_{(x: A)} Q(x)\right)$$

同样通过开“洞”可以看出：

$$f(p) :\equiv \left(\lambda x.\ \mathrm{pr}_1(p(x)), \lambda x.\ \mathrm{pr}_2(p(x))\right)$$

在类型论中使用宇宙实际上可以表达“高阶逻辑”。如下命题表达对任意性质 $P: A \to \mathcal{U}$, 若 $P(a)$ 则 $P(b)$:

$$\left(\prod_{P: A \to \mathcal{U}_i} P(a) \to P(b)\right): \mathcal{U} _{i+1}$$

由于这个逻辑（构造主义逻辑）是与证明有关（proof-relevant）的（例如说，如果有一个 $A + B$ 的实例，那么我们知道它来自于 $A$ 还是 $B$）。按照定义可以验证 $\N \iff \mathbf{1}$ 是正确的。这只是告诉我们当看成纯命题时，它们表达的命题是相同（这里是“真”命题）。当 $A \iff B$ 时我们称它们**逻辑相等**。

我们把“命题作为类型”反过来，任何类型可以看作命题，并以给出一个元素的方式证明。当我们给出一个元素时，称 $A$ 被居留；反之，给出 $\neg A$ 的一个元素时称 $A$ 不被居留。$\mathbf{0}$ 不被居留，因为 $\mathrm{id}_{\mathbf{0}}: \neg \mathbf{0}$. [^consistent]

## 相等类型
### refl
$a, b: A$ 的命题相等由类型来表达。我们把相等类型（identity type）的类型类写作（或简记为作 $a =_A b$，下标可省略）：

$$\mathrm{Id}_A: A \to A \to \mathcal{U}$$

正如把命题作为类型时，其元素含有的信息比“这个命题是正确的”本身要多，类型 $a = b$ 也含有更多的信息（这是同伦解释的基石）。

引入规则给出了表示自反性（reflexivity）的依值函数：

$$\mathrm{refl}: \prod_{a: A} (a =_A a)$$

如果 $a$ 与 $b$ 是判值相等的 $a \equiv b$，那么 $a =_A b$ 与 $a =_A a$ 是判值相等的，所以就有命题相等 $\mathrm{refl}_a: a =_A b$.

相等类型的归纳原则是类型论相当巧妙的部分。先考虑它的一个结论，称为不可区分同一性原理（Indiscernibility of identicals）：

对任意类型类 $C: A \to \mathcal{U}$ 存在一个函数：

$$f: \prod_{(x, y: A)} \prod_{(p: x =_A y)} C(x) \to C(y)$$

满足：

$$f(x, x, \mathrm{refl}_x) :\equiv \mathrm{id} _{C(x)}$$

### 路径归纳 {#path-induction}
相等类型的归纳原则被称为 J 原理或路径归纳（path induction）：

$$\mathrm{J}: \prod _{C: \prod _{x, y: A} x = y \to \mathcal{U}} \left[\prod _{c: A} C\ c\ c\ \mathrm{refl} _c\right] \to \prod _{x, y: A} \prod _{p: x = y} C\ a\ b\ p$$

也就是说，对类 $C: \prod_{x, y: A} (x =_A y) \to \mathcal{U}$ 及函数 $c: \prod _{x: A} C(x, x, \mathrm{refl}_x)$ 存在：

$$f: \prod_{(x, y: A)} \prod_{(p: x =_A y)} C(x, y, p)$$

满足：

$$f(x, x, \mathrm{refl}_x) :\equiv c(x)$$

关于把它打包成归纳函数的结果及 Based path induction 省略。

{% admonition(type="example", title="对称性") %}
$$\mathrm{sym}: (x = y) \to (y = x)$$
{% end %}

对 $p: x = y$ 取 $C(x, y, p)$ 是 $(y = x)$，$c(x)$ 是 $\mathrm{refl}_x$ 使用 J 原理即可。其模式匹配的结果（在 Agda 下）就是 [`sym refl = refl`](@/posts/agda_1.md).

{% admonition(type="example", title="传递性") %}
$$\mathrm{trans}: (x = y) \to (y = z) \to (x = z)$$
{% end %}

对 $p: x = y, q: y = z$ 取 $C(x, y, p)$ 是 $\prod_{(z: A)} (y = z) \to (x = z)$ 使用 J 原理即可。模式匹配写成 `trans sym q = q`.

### 不相等
我们定义 $(x \neq_A y) :\equiv \neg (x =_A y)$，此时称 $x$ 与 $y$ 不相等（unequal）。同之前一样，不相等的否定无法推出相等。

---

[^COQ92]: Thierry Coquand, "The Paradox of Trees in Type Theory," *BIT Numerical Mathematics* 32, no. 1 (1992): 10-14.
[^contradiction]: 如果我们的类型论是不自洽（inconsistent）的，则可能存在构造 $\mathbf{0}$ 的元素的复杂方法。
[^consistent]: 这不应与类型论是自洽的混淆。

+++
title = "【类型论】同伦类型论综述与 Martin-Löf 类型论"
date = 2026-03-09

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "类型论"]
+++

发现到时候不一定要读数院的研，考虑可以做逻辑学或者是 AI for math. 仔细阅读本书 [Homotopy Type Theory: Univalent Foundations of Mathematics](https://homotopytypetheory.org/book/). 这是上学期计概老师推荐的，但当时看得太粗略了。本文包含该书的前言和第一章内容。

<!-- more -->

开始写本文时还没有发现 [∞-type Café 暑期学校 2023](https://infinity-type-cafe.github.io/ntype-cafe-summer-school/) 的存在。

假定读者有简单类型论的基础。

## 前言
前言大致说：
> 同伦论发展于代数拓扑与同伦代数，与高阶类型论有一定关系。同伦类型论带来数学建立的新想法：一方面有 Voevodsky 的 univalence axiom 说我们可以识别出同构结构；另一方面提供了高阶归纳类型，更*能抓住人类直觉*。同伦类型论 HoTT 尚是新的领域[^written-time]，最终的理论也许和书中不一样。
>
> 类型论最早由 Bertrand Russell 发明，经很多人的推进（主要是 Church 及 Martin-Löf）得到了依值、构造性、直觉[^intuitionistic]的 Martin-Löf 类型论。这是我们所考虑的系统的基础。
>
> 在类型论中，对象由类型 `type` *分类*。项 `term` $a$ 有类型 $A$ 记作 $a: A$. 有精细结构的类型可以用于表达分类的细节。如积类型 $A \times B$, 函数类型 $A \to B$.
>
> 由于类型并*没有那么像*集合，新的观点是看作空间或者高阶广群。这些空间被纯同伦地处理而不是拓扑地处理。因此更精确地说我们把类型看作 $\infty$-广群。但我们仍可使用“空间”、“道路”这样的词汇。
>
> 把类型看作有结构的对象而非集合早有由来。如把类型看作层帮助解释类型论逻辑的直觉本性。同伦理解遵循了同样的模式：*澄清* the nature of 同一 identity (or 相等 equality) in type theory.
>
> 同伦理解的核心想法是把 $a = b$ 其中 $a, b: A$ 理解为空间 $A$ 中存在路径 $p: a \leadsto b$. 这也意味着 $f, g: A \to B$ 如果同伦可以识别，因为同伦就是一族 $B$ 中道路 $p_x: f(x) \leadsto g(x)$ 其中 $x: A$. 类型论中有一个（先前神秘的）类型 $\mathrm{Id}_A$ 就是道路空间 $A^I$ 包含从单位区间出发的连续映射 $I \to A$, 如此一来项 $p: \mathrm{Id}_A(a, b)$ 表达了 $A$ 中一个道路 $p: a \leadsto b$.
>
> （大段发展史省略）
>
> 类型论中可以有一个元素都是类型的类型，称作宇宙 `universe` 通常记作 $\mathcal{U}$. 其中的类型通常称为*小*[^small]类型。那什么是 $\mathcal{U}$ 中的道路 $p: A \leadsto B$ 呢？univalence axiom 就是说这样的道路对应于同伦等价 $A \simeq B$. 更准确地说，对小类型 $A, B$ 除了基础类型 $\mathrm{Id}_\mathcal{U}(A, B)$ 还有定义的 $\mathrm{Equiv}(A, B)$. 有典范的映射：
>
> $$\mathrm{Id}_\mathcal{U}(A, B) \to \mathrm{Equiv}(A, B)$$
>
> univalence axiom 陈述这个映射自身是一个等价，简化来说可以陈述为：
>
> $$(A = B) \simeq (A \simeq B)$$
>
> （省略进一步解释）

后面的内容就非常专业，考虑到知识有限且正文会给出一些详细的解释，此处从略。

## 类型论
### 概述
我们需要先提一点关于推演系统 `deductive system` 的事情。不正式地说，推演系统是用于推导出**判断** `judgments` 的一套规则 `rules`. 从逻辑观点看，判断可以被认为是*外在的*存在于*元理论*中的陈述。

在一阶逻辑（可以建立集合论）中只有一种判断：一个命题有证明。一个一阶逻辑的规则实际上是一种*证明构造*的规则。而类型论的基本判断是 $a: A$. 当 $A$ 表达的是命题时，$a$ 被称作 a *witness* to the provability of $A$ 或 *evidence* of the truth of $A$. 尽管 $a: A$ 与集合上说 $a \in A$ 有一些相似，前者是判断而后者是证明。我们不能说如果 $a: A$ 就没有 $b: B$, 也不能*证否*判断 $a: A$. 可以这样理解：类型论中类型对对象是本性的存在，在引入时就要声明。

类型论中处理等同的方式也与集合论不同。对 $a, b: A$ 如果类型 $a =_A b$ is inhabited 就说它们**命题相等**。与此同时我们也需要关于相等的判断，称为**判断相等/定义相等**，写作 $a \equiv b : A$ 或者简写 $a \equiv b$. 例如说对 $f: \mathbb{N} \to \mathbb{N}, f(x) = x^2$ 有 $f(3) = 3^2$ by definition. 这样的相等只是从定义延申出来的，尽管需要一种外在的算法来判断。

到现在为止，类型论的判断只有两种：一种形如 $a: A$, 另一种形如 $a \equiv b : A$.

我们现在使用符号 $:\equiv$ 来引入定义相等，如上例写成 $f(x) :\equiv x^2$. 这并不会产生符号上的歧义。

判断可能会基于形如 $x: A$ 的假设 `assumptions`. 例如说有 $x, y: A$ 及 $p: x =_A y$ 我们可以构造一个元素 $p^{-1}: y =_A x$. 这样的一组假设称为上下文 `context`. 技术上讲上下文需要是一个有序列表。

对命题 $A$ 及假设 $x: A$, 它是一个类型论版的假设 `hypothesis`.

一个 assumption 可以假定一个命题相等，但是不能假定判断相等，因为 $x \equiv y$ 并不是一个类型，并不可以有元素。有时对于一个变量 $x: A$ 我们可以替换为另一个具体的对象 $a: A$, 用词说“我们不妨设 $x \equiv a$”，尽管技术上这并不是 assumption.

类似地我们也不能证明一个判断相等。有时我们说“存在一个 $f: A \to B$ 使得 $f(x) \equiv y$”，应该理解成两个分离的判断，一个是 $f: A \to B$, 另一个是 $f(x) \equiv y$.

这一章的后面部分会非正式地给出类型论的表示[^formal]，只包含规则而没有公理 `axioms`.

### 函数类型
我们知道对函数 $f: A \to B$ 可以将它作用 `apply` 于域 $A$ 的元素 $a$ 得到陪域 $B$ 的元素，称为 $f$ 在 $a$ 的值。在类型论中常省略括号写作 $f\ a$. 但我们如何构造 $f: A \to B$ 的元素呢？

一种是直接定义，给出 $f(x) :\equiv \Phi$, 其中 $\Phi$ 是一个可能用到 $x$ 的表达式（需验证类型）。如果不希望给函数引入一个名字则可用 `λ-abstraction` 如：

$$(\lambda (x: A).\ \Phi): A \to B$$

这里 $x: A$ 可推断，故可省略为 $\lambda x.\ \Phi$ 或者写为 $x \mapsto \Phi$. 我们有时用空白 $-$ 来表达变量位置，如 $g(x, -)$ 表示 $\lambda y.\ g(x, y)$.

我们现在有以下**计算规则**，对应于 `β-reduction`, 其中 $\Phi'$ 是把 $\Phi$ 中所有 $x$ 替换为 $a$.

$$(\lambda x.\ \Phi)(a) = \Phi'$$

另外考虑称为 `η-expansion` 的相等：

$$f \equiv (\lambda x.\ f(x))$$

在表达式替换的过程中要注意同名问题。例如说对 $f(x) :\equiv \lambda y.\ x+y$ 考虑 $f(y)$ 是什么。如果变成 $\lambda y.\ y+y$ 那么就是 $y$ 被捕获 `capture` 了，这不是我们想要的。像 $\lambda y.\ x+y$ 中的 $y$ 是一个受限/哑变量 `bound/dummy variable`, 只有局部的含义。通过称为 `α-conversion` 的判断相等 $(\lambda y.\ x+y) \equiv (\lambda z.\ x+z)$ 我们有 $f(y)$ 与 $\lambda z.\ x+z$ 判断相等。

想要定义多变量的函数，可以使用之后引入的积类型。也可通过 `currying`. 我们把 $f: A \to (B \to C)$ 与 $f(a)(b)$ 简写为 $f: A \to B \to C$ 与 $f(a, b)$ 或去掉括号的 $f\ a\ b$. 定义时可以写 $f :\equiv \lambda x.\ \lambda y.\ \Phi$ 或 $f :\equiv x \mapsto y \mapsto \Phi$.

### 宇宙与类
在朴素集合论中并不能有一个包含所有类型，包括它自己的类型 $\mathcal{U}_\infty$. 这会导致悖论。[^COQ92]为此我们引入一组宇宙的层次：

$$\mathcal{U}_0: \mathcal{U}_1: \mathcal{U}_2: \cdots$$

这些宇宙是累积的 `cumulative`, 即 $\mathcal{U}_i$ 的元素也是 $\mathcal{U} _{i+1}$ 的元素。不过确会引入一些麻烦。

我们说 $A$ 是一个类型，如果它在某个宇宙 $\mathcal{U}_i$ 中。我们通常想要避免特别指出 $i$ 是什么，假设可以正确给出层级，写作 $A: \mathcal{U}$. 这种写法被称作 `typical ambiguity`. 当我们指定某个宇宙 $\mathcal{U}$ 时，称它含有的类型是小类型 `small types`. 为了建模一组关于类型 $A$ 变动的类型，我们使用 $B: A \to \mathcal{U}$. 这些函数称为类型类 `families of types` 或依值类型 `dependent types`. 一个例子是 $\mathrm{Fin}: \mathbb{N} \to \mathcal{U}$ 其中 $\mathrm{Fin}(n)$ 是一个恰有 $n$ 个元素的类型。一个平凡的例子是 $(\lambda (x: A).\ B): A \to \mathcal{U}$. 而反例是并没有类型类 $\lambda (i: \mathbb{N}).\ \mathcal{U}_i$.

### 依值函数类型
类型论中我们使用函数类型的更普遍版本：依值函数类型 `Π-type/dependent function type`. 依值函数的陪域类型可以随输入的值变动。对 $A: \mathcal{U}$ 及类 $B: A \to \mathcal{U}$ 我们将对应的依值函数类型写作：

$$\prod_{(x: A)} B(x) \text{\quad or \quad} \Pi (x: A), B(x)$$

如果 $B$ 是常值的，就有 $\prod_{(x: A)} B(x) \equiv (A \to B)$. 后文中会给出例子 $\mathrm{fmax}: \prod_{(n: \mathbb{N})} \mathrm{Fin}(n+1)$ 如何构造。

现在我们可以定义多态 `polymorphic` 函数。这是说先传入一个参数表示类型，然后作用于这个类型的元素。例如：

$$\mathrm{id}: \prod_{(A: \mathcal{U})} A \to A$$

$$\mathrm{id} :\equiv \lambda (A: \mathcal{U}).\ \lambda (x: A).\ x$$

定义也可简写为 $\mathrm{id}_A(x) :\equiv x$. 而使用 $\mathrm{id}(a)$ 是无歧义的。

一个更不那么平凡的例子是：

$$\mathrm{swap}: \prod_{(A: \mathcal{U})} \prod_{(B: \mathcal{U})} \prod_{(C: \mathcal{U})} (A \to B \to C) \to (B \to A \to C)$$

$$\mathrm{swap}_{A, B, C}(g)(b, a) :\equiv g(a, b)$$

### 积类型
对类型 $A, B: \mathcal{U}$ 我们引入类型 $A \times B: \mathcal{U}$. 其元素为满足 $a: A, b: B$ 的 $(a, b)$. 另外引入空积类型 `nullary product type` 或曰单位类型 `unit type` $\mathbf{1}: \mathcal{U}$, 其唯一元素 $\star: \mathbf{1}$. 不同于集合论，有序对在类型论是和函数一样的基础概念。

{% admonition(type="info", title="关于引入新类型") %}
在规定一个类型时，我们规定：
1. 如何形成这个类型：通过形成规则 `formation rules`, 如用类型 $A, B$ 形成 $A \to B$
2. 如何构造这个类型的元素：称为构造器/引入规则 `constructors/introduction rules`, 如 $\lambda x.\ 2x$
3. 如何使用这个类型的元素：称为 `eliminators/elimination rules`, 例如函数类型有一个 eliminator 是函数应用
4. 一个计算规则，表达 eliminator 如何用于一个 constructor
5. 一个可选的 uniqueness principle, 用于表达 uniqueness of maps into or out of that type, 例如判断相等 $f \equiv \lambda x.\ f(x)$; 有时并没有判断相等层面的这个规则，但可以由其它规则推出命题相等，此时称为 `propositional uniqueness principle`
{% end %}

所以我们如何*使用*有序对呢？对于 $f: A \times B \to C$, 我们可以发展出一个 elimination rule: 对每个 $g: A \to B \to C$ 可以定义出一个函数 $f: A \times B \to C$ 为 $f((a, b)) :\equiv g(a)(b)$.

也就是说，在类型论中，一个 $A \times B$ 上的函数在我们确定了其在有序对处的值时立即变得良定义，以此（更准确地说是从它的更一般的版本）我们能够*证明* $A \times B$ 的元素是有序对。从范畴论视角看我们可以说 $(-) \times B$ 是 $B \to (-)$ 的左伴随 `left adjoint`.

我们可以从这个 elimination rule 推导出投射函数 $\mathrm{pr}_1((a, b)) :\equiv a$ 与 $\mathrm{pr}_2((a, b)) :\equiv b$.

为了减少避免每次都使用这个规则，我们可以定义一个函数：

$$\mathrm{rec}_{A \times B}: \prod _{C: \mathcal{U}} (A \to B \to C) \to A \times B \to C$$

$$\mathrm{rec}_{A \times B}(C, g, (a, b)) :\equiv g(a)(b)$$

之后就可以像 $\mathrm{pr}_1 :\equiv \mathrm{rec} _{A \times B}(A, \lambda a.\ \lambda b.\ a)$ 一样使用。

我们称 $\mathrm{rec}_{A \times B}$ 是积类型的递归函数 `recursor`. 这里没有明显体现递归，因为积类型是归纳类型的一个退化例子。

为了能够在积类型上定义依值函数，我们必须把递归函数一般化。可以从下式中通过 $f((x, y)) :\equiv g(x)(y)$ 定义出一个 $f: \prod_{(x: A \times B)} C(x)$:

$$g: \prod_{(x: A)} \prod_{(y: B)} C((x, y))$$

由此我们可以构建一个函数：

$$\mathrm{uniq}_{A \times B}: \prod _{x: A \times B} ((\mathrm{pr}_1(x), \mathrm{pr}_2(x))) = _{A \times B} x$$

关于 identity type 具体的内容会在之后的节中介绍。现在我们只需知道对 $x: A$ 有 $\mathrm{refl}_x: x=_A x$. 由此可以定义：

$$\mathrm{uniq}_{A \times B}((a, b)) :\equiv \mathrm{refl} _{(a, b)}$$

这是因为存在判断相等 $(\mathrm{pr}_1((a, b)), \mathrm{pr}_2((a, b))) \equiv (a, b)$.

更进一步可以有 induction 函数：

$$\mathrm{ind}_{A \times B}: \prod _{C: A \times B \to \mathcal{U}} \left(\prod _{(x: A)} \prod _{(y: B)} C((x, y))\right) \to \prod _{x: A \times B} C(x)$$

可见 recursor 只是 induction 的一个特例。我们也称 induction 为  (dependent) eliminator, 称 recursion 为 non-dependent eliminator.

关于单位类型的讨论从略。

### 依值对类型
我们扩展积类型到依值对类型 `Σ-type/dependent pair type`. 这对应于集合论中一个指标集对应一组集合的无交并。给定类型 $A: \mathcal{U}$ 与类 $B: A \to \mathcal{U}$, 写作：

$$\sum_{(x: A)} B(x) \text{\quad or \quad} \Sigma (x: A), B(x)$$

其构造方法也是通过对 $a: A$ 与 $b: B(a)$ 配对 $(a, b)$.

我们写出第一个的投射函数为：

$$\mathrm{pr}_1: \left(\sum _{(x: A)} B(x)\right) \to A$$

$$\mathrm{pr}_1((a, b)) :\equiv a$$

第二个投射函数需要是一个依值函数，类型为：

$$\mathrm{pr}_2: \prod _{p: \sum _{x: A} B(x)} B(\mathrm{pr}_1(p))$$

为此我们需要一个用于 Σ-type 的 induction principle. 也即希望从 $C: (\sum_{x: A} B(x)) \to \mathcal{U}$ 及：

$$g: \prod_{a: A} \prod_{b: B(a)} C((a, b))$$

推导出：

$$f: \prod _{p: \sum _{x: A} B(x)} C(p)$$

由于与积类型的情况类似，此处省略 recursor 与 induction 的内容。

作为一个有趣的例子，对类型 $A, B$ 及 $R: A \to B \to \mathcal{U}$ 考察：

$$\mathrm{ac}: \left(\prod_{(x: A)} \sum_{(y: B)} R(x, y)\right) \to \left(\sum_{(f: A \to B)} \prod_{(x: A)} R(x, f(x))\right)$$

$$\mathrm{ac}(g) :\equiv \left(\lambda x.\ \mathrm{pr}_1(g(x)), \lambda x.\ \mathrm{pr}_2(g(x))\right)$$

如果把 $\prod$ 读作“对任意”，把 $\sum$ 读作“存在”，那么这个类型表示：如果对任意 $x: A$ 存在一个 $y: B$ 满足 $R(x, y)$, 那么存在一个函数 $f: A \to B$ 使得对任意 $x: A$ 有 $R(x, f(x))$. 由于这和集合论中的选择公理比较像，这个函数传统上称为 type-theoretic axiom of choice.

依值对类型通常用于表达一些有多片数据的数学结构。例如一个原群 magma 是 $(A, m)$, 其中 $A: \mathcal{U}$ 及 $m: A \to A \to A$. 因此说：

$$\mathrm{Magma} :\equiv \sum_{A: \mathcal{U}} (A \to A \to A)$$

最后，为了方便起见，我们使用符号 $(x, y, z) :\equiv (x, (y, z))$ 等等。

### 余积类型
对 $A, B: \mathcal{U}$ 可以有余积类型 `coproduct type` $A + B: \mathcal{U}$ 对应于集合论中的无交并。我们也引入一个空版本：空类型 `empty type` $\mathbf{0}: \mathcal{U}$.

$A + B$ 的元素有左右嵌入 `injection` 两种构造方法:对 $a: A$ 的 $\mathrm{inl}(a): A + B$ 与对 $b: B$ 的 $\mathrm{inr}(b): A + B$.

为了构造非依值函数 $f: A + B \to C$, 需要函数 $g_0: A \to C$ 与 $g_1: B \to C$, 然后写：

$$
f(\mathrm{inl}(a)) :\equiv g_0(a) \\\\
f(\mathrm{inr}(b)) :\equiv g_1(b)
$$

这种定义称为 case analysis. 如之前一样我们可以推导出 recursor 和 induction.

特别地 $\mathbf{0}$ 的 recursor 是 $\mathrm{rec}_\mathbf{0}: \prod _{C: \mathcal{U}} \mathbf{0} \to C$. 这对应于经典的 *ex falso quodlibet* 原则：从矛盾中可以推出任何命题。

### 布尔类型
布尔类型 `boolean` 希望是形如 $0_{\mathbf{2}}, 1_{\mathbf{2}}: \mathbf{2}$. 它可以由 $\mathbf{1} + \mathbf{1}$, 但为方便起见给出特别的讨论。

recursor 对应于 if-then-else 结构：

$$\mathrm{rec} _{\mathbf{2}}: \prod _{C: \mathcal{U}} C \to C \to \mathbf{2} \to C$$

$$\mathrm{rec} _{\mathbf{2}}(C, c_0, c_1, 0 _{\mathbf{2}}) :\equiv c_0$$

$$\mathrm{rec} _{\mathbf{2}}(C, c_0, c_1, 1 _{\mathbf{2}}) :\equiv c_1$$

把它打包成 induction principle 如下：

$$\mathrm{ind}_{\mathbf{2}}: \prod _{(C: \mathbf{2} \to \mathcal{U})} C(0 _{\mathbf{2}}) \to C(1 _{\mathbf{2}}) \to \prod _{x: \mathbf{2}} C(x)$$

通过传入 $\mathrm{inl}(\mathrm{refl} _{0 _\mathbf{2}})$ 与 $\mathrm{inr}(\mathrm{refl} _{1 _\mathbf{2}})$ 可以构造出一个：

$$\prod_{x: \mathbf{2}} (x = 0_\mathbf{2}) + (x = 1_\mathbf{2})$$

布尔类型也可以用于定义二元的无交并（同集合论中所用的方法）：

$$A + B :\equiv \sum_{x: \mathbf{2}} \mathrm{rec}_{\mathbf{2}}(\mathcal{U}, A, B, x)$$

也可定义笛卡尔积：

$$A \times B :\equiv \prod_{x: \mathbf{2}} \mathrm{rec}_{\mathbf{2}}(\mathcal{U}, A, B, x)$$

我们常将 $0_{\mathbf{2}}, 1_{\mathbf{2}}$ 称作 `false` 与 `true`. 但我们不把 $\mathbf{2}$ 的元素视作真值或者命题。

### 自然数
自然数 $\mathbb{N}$ 的元素是通过 $0: \mathbb{N}$ 与 $\mathrm{succ}: \mathbb{N} \to \mathbb{N}$ 定义的。现在其上的 recursion 与 induction 更符合常用的含义了。

使用素材 $c_0: C$ 与 $c_s: \mathbb{N} \to C \to C$ 可以定义：

$$f(0) :\equiv c_0$$

$$f(\mathrm{succ}(n)) :\equiv c_s(n, f(n))$$

这称为通过原始递归 `primitive recursion` 定义。通过原始递归定义的*显然*是*可计算的 computable*.

把原始递归打包成 recursor:

$$\mathrm{rec}_{\mathbb{N}}: \prod _{C: \mathcal{U}} C \to (\mathbb{N} \to C \to C) \to \mathbb{N} \to C$$

同样一般化为归纳法 induction principle:

$$\mathrm{ind}_{\mathbb{N}}: \prod _{(C: \mathbb{N} \to \mathcal{U})} C(0) \to \left(\prod _{(n: \mathbb{N})} C(n) \to C(\mathrm{succ}(n))\right) \to \prod _{(n: \mathbb{N})} C(n)$$

一个自然数的性质 `property` 是由类型类 $P: \mathbb{N} \to \mathcal{U}$ 表达的。由此，上式即是数学归纳法。

### 模式匹配与递归
我们有时会这样定义函数：

$$
f(\mathrm{inl}(a)) :\equiv \Phi_0 \\\\
f(\mathrm{inr}(b)) :\equiv \Phi_1
$$

这本质上是使用了 recursor:

$$f :\equiv \mathrm{rec}_{A+B}(C, \lambda a.\ \Phi_0, \lambda b.\ \Phi_1)$$

而对：

$$
f(0) :\equiv \Phi_0 \\\\
f(\mathrm{succ}(n)) :\equiv \Phi_s 
$$

则本质上是：

$$f :\equiv \mathrm{rec}_{\mathbb{N}}(C, \Phi_0, \lambda n.\ \lambda r.\ \Phi_s')$$

这种使用 recursion/induction 非常方便，称为模式匹配 `pattern matching`. 当然它是有局限的，例如说 $f(\mathrm{succ}(n)) :\equiv$ 的右侧只能出现 $f(n)$ 而不能出现 $f(\mathrm{succ}(\mathrm{succ}(n)))$ 这样的东西。

### 命题作为类型
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

而如 if-and-only-if $A \iff B$ 这样的可以组合表达为 $(A \to B) \times (B \to A)$.

我们把 $\mathbf{0}$ 的实现称为一个矛盾 `contradiction`. 并不存在证明一个矛盾的方法。[^contradiction]

现在考虑实现 $(A \to \mathbf{0}) \times (B \to \mathbf{0}) \to (A + B \to \mathbf{0})$. 我们可以开“洞”然后整理：

$$
f((x, y))(\mathrm{inl}(a)) :\equiv \Box : 0 \\\\
f((x, y))(\mathrm{inr}(b)) :\equiv \Box : 0
$$

此时就可以看出两个洞应该填入 $x(a)$ 与 $y(b)$.

现在我们无法构造 $((A \to \mathbf{0}) \to \mathbf{0}) \to A$ 的实例，因为在直觉主义逻辑中并不成立它（双重否定消除）。直觉主义中并不存在经典逻辑的排中律 `LEM/law of excluded middle` 或者通过矛盾来证明。需要额外引入公理或者 `call/cc`. 反过来的双重否定引入则是可以构造实例的。

现在考虑一个带谓词的逻辑对应的例子：

$$\left(\prod_{(x: A)} P(x) \times Q(x)\right) \to \left(\prod_{(x: A)} P(x)\right) \times \left(\prod_{(x: A)} Q(x)\right)$$

同样通过开“洞”可以看出：

$$f(p) :\equiv \left(\lambda x.\ \mathrm{pr}_1(p(x)), \lambda x.\ \mathrm{pr}_2(p(x))\right)$$

在类型论中使用宇宙实际上可以表达“高阶逻辑”。如下命题表达对任意性质 $P: A \to \mathcal{U}$, 若 $P(a)$ 则 $P(b)$:

$$\left(\prod_{P: A \to \mathcal{U}_i} P(a) \to P(b)\right): \mathcal{U} _{i+1}$$

由于这个逻辑（构造主义逻辑）是与证明有关 `proof-relevant` 的（例如说，如果有一个 $A + B$ 的实例，那么我们知道它来自于 $A$ 还是 $B$）。按照定义可以验证 $\mathbb{N} \iff \mathbf{1}$ 是正确的。这只是告诉我们当看成纯命题时，它们表达的命题是相同（这里是“真”命题）。当 $A \iff B$ 时我们称它们**逻辑相等**。

我们把“命题作为类型”反过来，任何类型可以看作命题，并以给出一个元素的方式证明。当我们给出一个元素时，称 $A$ is `inhabited`. 反之，给出 $\neg A$ 的一个元素时称 $A$ is not inhabited. 易见 $\mathbf{0}$ is not inhabited, 因为 $\mathrm{id}_{\mathbf{0}}: \neg \mathbf{0}$. [^consistent]

### 恒等类型
$a, b: A$ 的相等性必须有某个类型。我们把恒等类型 `identity type` 的类型类写作：

$$\mathrm{Id}_A: A \to A \to \mathcal{U}$$

把类型 $\mathrm{Id}_A(a, b)$ 简记作 $a = b$.

正如把命题作为类型时，其元素含有的信息比“这个命题是正确的”本身要多，类型 $a = b$ 也含有更多的信息（这是同伦解释的基石）。

引入规则是这样一个依值函数：

$$\mathrm{refl}: \prod_{a: A} (a =_A a)$$

这称为自反性 `reflexivity`. 如果 $a$ 与 $b$ 是判断相等的 $a \equiv b$, 那么 $a =_A b$ 与 $a =_A a$ 是判断相等的，所以可以说 $\mathrm{refl}_a: a =_A b$.

恒等类型的 induction principle 是类型论相当巧妙的部分。先考虑它的一个结论，称为不可区分同一性原理 Indiscernibility of identicals:

对任意类型类 $C: A \to \mathcal{U}$ 存在一个函数：

$$f: \prod_{(x, y: A)} \prod_{(p: x =_A y)} C(x) \to C(y)$$

满足 $f(x, x, \mathrm{refl}_x) :\equiv \mathrm{id} _{C(x)}$.

---

恒等类型的 induction principle 被称为 path induction. 可以被视作是说恒等类型类是由形如 $\mathrm{refl}_x$ 的元素生成的。

path induction 的内容如下：对类 $C: \prod_{x, y: A} (x =_A y) \to \mathcal{U}$ 及函数 $c: \prod _{x: A} C(x, x, \mathrm{refl}_x)$ 存在：

$$f: \prod_{(x, y: A)} \prod_{(p: x =_A y)} C(x, y, p)$$

满足 $f(x, x, \mathrm{refl}_x) :\equiv c(x)$.

为了理解它，先考虑 $C$ 不依赖于 $p$ 的简化情形。此时 $C: A \to A \to \mathcal{U}$ 可以看作一个依赖两个元素的谓词。此时 path induction 说如果对所有 $x: A$ 成立 $C(x, x)$ 那么当 $x = y$ 时成立 $C(x, y)$.

而让 $C$ 依赖于 $p$ 对后面的同伦解释是有用的。

把它打包成一个函数 $\mathrm{ind}_{=_A}$ 的结果省略。

{% admonition(type="warning", title="Based path induction") %}
此处省略关于 Based path induction 与它和 path induction 相等性的讨论。考虑并入之后章节的笔记。
{% end %}

---

我们定义 $(x \neq_A y) :\equiv (x =_A y)$. 此时称 $x$ 与 $y$ 不相等 `unequal`. 同之前一样，并非不相等无法推出相等。

---

[^written-time]: 注：本书似乎是 2013 年出版的。在此之后一个比较有名的是立方类型论 Cubical Type Theory, 暂未详细了解。
[^intuitionistic]: 应当是指直觉主义那种。不考虑哲学细节，直觉主义和构造主义可以认为是相同的。Brouwer-Heyting-Kolmogorov 释义说我们构造出事物而不是使用真值表。参考 <https://ncatlab.org/nlab/show/BHK+interpretation>.
[^small]: 看起来和集合论的称法是一样的。集合论有些书也称 $\mathcal{U}$-小的。
[^formal]: 正式版本被放在附录 A. 包含了严格、完整的表述。
[^COQ92]: Thierry Coquand. The paradox of trees in type theory. *BIT Numerical Mathematics*, 32(1):10–14, 1992.
[^contradiction]: 如果我们的类型论是*不一致 inconsistent* 的，则可能存在构造 $\mathbf{0}$ 的元素的复杂方法。
[^consistent]: 这不应与类型论是一致的 consistent 混淆。一致是说通过类型论规则无法得到 $\mathbf{0}$ 的元素。

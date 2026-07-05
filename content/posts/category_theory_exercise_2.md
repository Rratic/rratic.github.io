+++
title = "Universals and Limits 习题选做"
date = 2026-07-05

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "范畴论"]
+++

做一些 Mac Lane 书上 Ⅲ. Universals and Limits 的习题（从 2. The Yoneda Lemma 开始，前面的在[一些泛性质使用的习题选做](@/posts/category_theory_exercise_1.md)）。

<!-- more -->

我们回顾 Yoneda Lemma 是指：

{% admonition(type="theorem", title="Yoneda Lemma") %}
对范畴 $\mathcal{D}$ 有小 hom-sets 及其中对象 $r$ 与函子 $K: \mathcal{D} \to \mathbf{Set}$，存在双射：

$$
\begin{aligned}
\Phi: \mathrm{Nat}(D(r, -), K) & \cong Kr \cr
    \alpha & \mapsto \alpha_r(\mathbf{1}_r)
\end{aligned}
$$
{% end %}

如图可知我们能找到唯一的原像。

$$
\begin{CD}
   D(r, r) @>\alpha_r>> K(r) \cr
   @VD(r, f)VV @VK(f)VV \cr
   D(r, d) @>\alpha_d>> K(d)
\end{CD}
$$

代入 $K = D(s, -)$，它给出推论 $\mathrm{Nat}(D(r, -), D(s, -)) \cong D(s, r)$，进而有：

$$D(r, -) \cong D(s, -) \iff r \cong s$$

{% admonition(type="question", title="Ⅲ 2.3 (Kan; the coyoneda lemma)") %}
For $K: \mathcal{D} \to \mathbf{Set}$, $(\ast \downarrow K)$ is the category of elements $x \in Kd$, $Q: (\ast \downarrow K) \to \mathcal{D}$ is the projection $x \in Kd \mapsto d$ and for each $a \in \mathcal{D}, a: (\ast \downarrow K) \to \mathcal{D}$ is the diagonal functor sending everything to the constant value $a$. Establish a natural isomorphism

$$\mathrm{Nat}(K, D(a, -)) \cong \mathrm{Nat}(a, Q)$$
{% end %}

更清晰地说，$(\ast \downarrow K)$ 中元素形如 $(d, x)$，对 $f: d \to d'$ 一个 $f_\ast: (d, x) \to (d', x')$ 满足 $K(f)(x) = x'$.

观察如下两个交换图，考虑 $\alpha$ 被映到 $\beta_{(d, x)}(a) = \overline{\alpha_d(x)}(a)$，这里上划线表示任取一个元素。逆是 $\alpha_d = \set{\beta_{(d, x)} | x \in K(d)}$.

$$
\begin{CD}
   K(a) @>\alpha_a>> D(a, a) @. a @>\beta_{(a, x)}>> a \cr
   @VK(f)VV         @VD(a, f)VV @VVV      @VfVV \cr
   K(b) @>\alpha_b>> D(a, b) @. a @>\beta_{(b, y)}>> b
\end{CD}
$$

{% admonition(type="question", title="Ⅲ 3.3") %}
In the category $\mathbf{Matr}_K$, describe the coequalizer of two $m \times n$ matrices $A, B$ (i.e., of two arrows $n \to m$ in $\mathbf{Matr}_K$).
{% end %}

在满足 $CA = CB$ 的矩阵 $C$ 中按泛性质选取。由于可在同构意义下考虑，设 $\mathrm{Im}(A - B) = \mathrm{span}\set{v_1, \dots, v_{\mathrm{rank}(A - B)}}$，则这些需要被映到 $0$. 考虑余等化子箭头是到 $\mathrm{Im}(A - B)^\perp$ 上的投影，余等化子对象是 $m - \mathrm{rank}(A - B)$ 知成立，从而容易描述所有的余等化子。

{% admonition(type="question", title="Ⅲ 3.5") %}
If $E$ is an equivalent relation on a set $X$, show that the usual set $X/E$ of equivalence classes can be described by a coequalizer in $\mathbf{Set}$.
{% end %}

我们回忆等价关系实际上是 $E \subseteq X \times X$. 我们有 $\pi_1, \pi_2: E \to X$，考虑它们的余等化子。

{% admonition(type="question", title="Ⅲ 3.7") %}
If $A$ is an abelian group, and $J_A$ the preorder with objects all finitely generated subgroups $S \subseteq A$ ordered by inclusion, show that $A$ is the colimit of the evident functor $J_A \to \mathrm{Ab}$. Generalize.
{% end %}

我们用下标表示那个 evident functor $F$ 及余极限箭头在指定索引处的值。

让余极限箭头 $u$ 是嵌入。若另有 $\braket{u', A'}$，我们希望找到 $f: A \to A'$ 使得 $f \circ u_i = u_i'$ 在每一 $i$ 处成立。

考虑 $F(i)$ 是单个生成元 $a_i$ 生成的，我们可以写出 $f$ 在 $a_i$ 处的取值。由任意性可以拼出整个 $f$，易见 $f$ 满足条件。也就是说 $A$ 确实是余极限对象。

结论的一般化略去。

{% admonition(type="question", title="Ⅲ 4.3") %}
If the category $J$ has an initial object $s$, prove that every functor $F: J \to C$ to any category $C$ has a limit, namely $F(s)$. Dualize.
{% end %}

先验证 $F(s)$ 是极限。对 $i$ 有唯一箭头 $s \to i$ 提升成 $u_i: F(s) \to F(i)$，图表显然交换。若另有 $\braket{u', G}$，我们希望找到 $f: G \to F(s)$ 使得 $u_i \circ f = u_i'$ 在每一 $i$ 处成立。取 $f = u_s'$ 即可。

对偶形式略去。

{% admonition(type="question", title="Ⅲ 4.7") %}
If $C$ has finite products and equalizers, show that the kernel pair of $f: a \to b$ may be expressed in terms of the projections $p_1, p_2: a \times a \to a$ as $p_1e, p_2e$, where $e$ is the equalizer of $fp_1, fp_2: a \times a \to b$. Dualize.
{% end %}

首先验证以下图表确实交换：

$$
\begin{CD}
   c @>p_1e>> a \cr
   @Vp_2eVV @VfVV \cr
   a @>f>> b
\end{CD}
$$

我们知道有唯一 $e: c \to a \times a$ 使图表交换。若另外有 $g_1, g_2: d \to a$ 使图表交换，则存在唯一 $e': d \to a \times a$ 使图表交换。现在对 $a \times a \rightrightarrows b$ 使用 equalizer 的泛性质知成立。

对偶形式略去。

{% admonition(type="question", title="Ⅲ 4.10") %}
If $C$ has pullbacks and a terminal object, prove that $C$ has all finite products and equalizers.
{% end %}

两个对象的积只需考虑 $A \to \mathbf{1}$ 与 $B \to \mathbf{1}$ 的拉回，从而有有限积。而 equalizer 则是 $\braket{1_A, f}, \braket{1_A, g}: A \to A \times B$ 的拉回。

{% admonition(type="question", title="Ⅲ 5.5") %}
If $B$ has (finite) products show that any functor category $B^C$ also has (finite) products (calculated “pointwise”).
{% end %}

对 $f, g: C \to B$，令 $h(c) = f(c) \times g(c)$ 即可。

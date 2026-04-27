+++
title = "关于滤子：极限及“最终”模态词诱导"
date = 2026-04-22
updated = 2026-04-27

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "分析", "拓扑"]
+++

起因是听说 Lean 现在的 mathlib 中实数是依靠滤子定义的（虽然实际搜索了一下发现当前版本还是改为用 Cauchy 序列定义）。 

<!-- more -->

用 [LeanSearch](https://leansearch.net/) 搜索到用 Cauchy 序列（[用 $\epsilon-\delta$ 语言定义](https://leansearch.net/doc/Mathlib.Algebra.Order.CauSeq.Basic.html#IsCauSeq)）定义的 [Data.Real.Basic - Real](https://leansearch.net/doc/Mathlib.Data.Real.Basic.html#Real) 与用 Cauchy 滤子定义的 [Topology.UniformSpace.CompareReals - CompareReals.Bourbakiℝ](https://leansearch.net/doc/Mathlib.Topology.UniformSpace.CompareReals.html#CompareReals.Bourbaki%E2%84%9D).

文档说使用前一定义是因：
> This choice is motivated by how easy it is to prove that `ℝ` is a commutative ring, by simply lifting everything to `ℚ`.

使用 Git Blame 追踪历史发现：
- 该定义直接导入自 mathlib3 [feat: port Data.Real.Basic](https://github.com/leanprover-community/mathlib4/commit/f20a2b3ac74386a73b9ea12721a3b2bd2f41dec4)
- 应当来自于 2018 年的更改 [feat(data/real): reals from first principles](https://github.com/leanprover-community/mathlib3/commit/04cac9587e1d32479935a0cf132b140b28bbac2b)

不过不管怎么说让我们了解一下滤子。

---

{% admonition(type="definition", title="滤子 filter") %}
集合 $X$ 上的滤子（filter）是 $\mathcal{F} \subseteq \mathcal{P}(X)$ 满足：
- 向上封闭：若 $A \in \mathcal{F}$ 且 $A \subset B$ 则 $B \in \mathcal{F}$
- 对有限交封闭：若 $A, B \in \mathcal{F}$ 则 $A \cap B \in \mathcal{F}$

当 $\emptyset \notin \mathcal{F}$ 时称它为真滤子。
{% end %}

滤子描述的是“满足所有要求”的元素。例如说对 $q \in \mathbb{Q}$ 它的邻域滤子是：

$$\set{A \in \mathbb{Q} | \exists \varepsilon: (q-\varepsilon, q+\varepsilon) \cap \mathbb{Q} \subseteq A}$$

因此，我们称一个滤子 $\mathcal{F}$ 比另一个细 $\mathcal{F} \leq \mathcal{G}$ 如果 $\mathcal{F} \supseteq \mathcal{G}$. 这里“细”意为更精确。最精确的是 $\bot = \mathcal{P}(X)$ 包含空集；最模糊的是 $\top = \set{X}$.

---

一个 $\mathbb{Q}$ 上的 Cauchy 滤子是满足对任意 $\varepsilon > 0$ 存在 $A \in \mathcal{F}$ 使得 $\operatorname{diam} A < \varepsilon$ 的滤子。记两个滤子 $\mathcal{F} \sim \mathcal{G}$ 等价，若 $\mathcal{F} \cap \mathcal{G}$ 仍是 Cauchy 滤子。

此时，可以将实数 $\R$ 定义为 Cauchy 滤子的等价类。

---

但实际上 mathlib 中考虑的是一般的定义，滤子是否 Cauchy 定义在一致空间上。

{% admonition(type="definition", title="一致空间 uniform space") %}
一致空间是指集合 $X$ 有一个额外的一致结构。

一致结构是一族二元关系 $\mathcal{U} \in \set{U_i \subseteq X \times X}_{i \in I}$, 其中二元关系意为“邻近”，满足：
- $\mathcal{U}$ 是 $X \times X$ 上的滤子
- 二元关系是自反的 $x \stackrel{U}{\sim} x$
- 二元关系是对称的 $x \stackrel{U}{\sim} y \iff y \stackrel{U}{\sim} x$
- 三角不等式：对任一二元关系 $U$ 存在另一二元关系 $V$, $x \stackrel{V}{\sim} y \stackrel{V}{\sim} z \implies x \stackrel{U}{\sim} z$
{% end %}

```lean
/-- A uniform space is a generalization of the "uniform" topological aspects of a
  metric space. It consists of a filter on `α × α` called the "uniformity", which
  satisfies properties analogous to the reflexivity, symmetry, and triangle properties
  of a metric.

  A metric space has a natural uniformity, and a uniform space has a natural topology.
  A topological group also has a natural uniformity, even when it is not metrizable. -/
class UniformSpace (α : Type u) extends TopologicalSpace α where
  /-- The uniformity filter. -/
  protected uniformity : Filter (α × α)
  /-- If `s ∈ uniformity`, then `Prod.swap ⁻¹' s ∈ uniformity`. -/
  protected symm : Tendsto Prod.swap uniformity uniformity
  /-- For every set `u ∈ uniformity`, there exists `v ∈ uniformity` such that `v ○ v ⊆ u`. -/
  protected comp : (uniformity.lift' fun s => s ○ s) ≤ uniformity
  /-- The uniformity agrees with the topology: the neighborhoods filter of each point `x`
  is equal to `Filter.comap (Prod.mk x) (𝓤 α)`. -/
  protected nhds_eq_comap_uniformity (x : α) : 𝓝 x = comap (Prod.mk x) uniformity
```

作为一个例子，度量可以诱导一个一致空间（指标 $r$ 对应二元关系是 $d(x, y) < r$）。

{% admonition(type="definition", title="Cauchy 滤子") %}
一个真滤子是一致空间 $X$ 上的真滤子，如果对任意 $f \in \mathcal{F}$, $f \times f$ 被包含在 $X$ 的一致结构的某个滤子中。
{% end %}

```lean
/-- A filter `f` is Cauchy if for every entourage `r`, there exists an
  `s ∈ f` such that `s × s ⊆ r`. This is a generalization of Cauchy
  sequences, because if `a : ℕ → α` then the filter of sets containing
  cofinitely many of the `a n` is Cauchy iff `a` is a Cauchy sequence. -/
def Cauchy (f : Filter α) :=
  NeBot f ∧ f ×ˢ f ≤ 𝓤 α
```

对序列 $a: \N \to X$ 诱导出尾部滤子：

$$f_a \coloneqq \set{A \subseteq X | \exists N, \set{a_n | n \geq N} \subseteq A}$$

可见若 $X$ 上的一致结构是度量诱导的，则 $f_a$ 是 Cauchy 滤子当且仅当 $a$ 是 Cauchy 序列。

{% admonition(type="definition", title="收敛") %}
称滤子 $\mathcal{F}$ 收敛到 $x$, 或者说 $x$ 是 $\mathcal{F}$ 的极限，如果 $\mathcal{F}$ 是 $x$ 的邻域滤子的加细。
{% end %}

{% admonition(type="definition", title="Tendsto") %}
对 $f: X \to Y$ 及 $X$ 上的滤子 $\mathcal{F}$, $Y$ 上的滤子 $\mathcal{G}$, 则 $f$ 沿着 $\mathcal{F}$ 趋近于 $\mathcal{G}$ 如果对任意 $G \in \mathcal{G}$ 有 $f^{-1}(G) \in \mathcal{F}$.
{% end %}

```lean
/-- `Filter.Tendsto` is the generic "limit of a function" predicate.
  `Tendsto f l₁ l₂` asserts that for every `l₂` neighborhood `a`,
  the `f`-preimage of `a` is an `l₁` neighborhood. -/
def Tendsto (f : α → β) (l₁ : Filter α) (l₂ : Filter β) :=
  l₁.map f ≤ l₂
```

作为一个例子，函数 $x \mapsto x^2$ 沿着 $2$ 的邻域滤子趋近于 $4$ 的邻域滤子，这等价于：

$$\lim_{x \to 2} x^2 = 4$$

仿照 $\varepsilon-\delta$ 语言的行为，读者容易写出描述趋向于正无穷和负无穷的极限过程的滤子（顶部滤子 `atTop` 与底部滤子 `atBot`）。

{% admonition(type="definition", title="一致连续") %}
对一致空间 $(X, \mathscr{U})$, $(Y, \mathscr{V})$, 称 $f: X \to Y$ 一致连续，若对任意 $V \in \mathscr{V}$ 有 $f^{-1}(V) \in \mathscr{U}$.
{% end %}

考察度量诱导的一致结构，读者可见它与一般的定义一致。

---

除了刻画极限外，滤子另一个用途是刻画“最终”概念：

```lean
/-- `f.Eventually p` or `∀ᶠ x in f, p x` mean that `{x | p x} ∈ f`. E.g., `∀ᶠ x in atTop, p x`
means that `p` holds true for sufficiently large `x`. -/
protected def Eventually (p : α → Prop) (f : Filter α) : Prop :=
  { x | p x } ∈ f

/-- `f.Frequently p` or `∃ᶠ x in f, p x` mean that `{x | ¬p x} ∉ f`. E.g., `∃ᶠ x in atTop, p x`
means that there exist arbitrarily large `x` for which `p` holds true. -/
protected def Frequently (p : α → Prop) (f : Filter α) : Prop :=
  ¬∀ᶠ x in f, ¬p x
```

我们可以把它们做成[模态逻辑](@/posts/logic_3.md)中的 $\Box$ 与 $\Diamond$.

容易发现它除了满足系统 $K$ 还会满足持续性 $D$ 公理 $\Box p \to \Diamond p$, 传递性 $4$ 公理 $\Box p \to \Box \Box p$, 欧性 $5$ 公理 $\neg \Box p \to \Box \neg \Box p$.

---

我自己的一点想法是，滤子可以用于知识逻辑。

先看一个一般的知识逻辑讨论：

{% admonition(type="note", title="Williamson (1992) 关于非精确知识的讨论") %}
假设现在有一棵离你比较远的树，无法精确判断它有多高，假设性质：

$$p_k: \text{这棵树} k \text{厘米高}$$

由于无法判断精确高度，不妨假定：我们知道，如果树 $k+1$ 厘米高的话，我们无法知道树不是 $k$ 厘米高，即：

$$K(p_{k+1} \to \neg K \neg p_k)$$

我们可以依次推出：
- 对任意 $k$ 有 $K(K(\neg p_k \to \neg p_{k+1}))$
- 假设 $K \neg p_0$ 则 $KK \neg p_0$, 进而 $K \neg p_1$
- 同上一步归纳，发现树多高都不可以
{% end %}

这指出的是我们从自然语言翻译到形式语言时的问题。这里的重点在于使用了正自省，对应到可能世界语义上，说的是主体不可区分世界的传递性。实际上，我们认为 $w_1$ 与 $w_2$ 不可区分且 $w_2$ 与 $w_3$ 不可区分，则不一定 $w_1$ 与 $w_3$ 不可区分（这里我定义 $V(p_k) = \set{w_k}$）。

为此，可能有一个修正是说引入概率，但是我想这并无助于加深理解。

我的想法是，一个可能世界除了包含树的真实高度还包含一个滤子 $\mathcal{F}_k^i$ 满足它可以加细成一个收敛到 $k$ 的滤子，含义是“观察能力”。观察者不能区分两个世界，如果一个滤子是另一个的加细。

现在，在 $(k, i)$-世界上有 $\neg K \neg p_k$.

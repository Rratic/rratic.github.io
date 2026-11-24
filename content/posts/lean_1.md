+++
title = "Lean 语言（一）：基础图景"
date = 2026-11-24

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["计算机"]
+++

因为参与于形式化实习，故进行一些 Lean 的系统学习。

<!-- more -->

关于 Lean 4 的安装可以参见 <https://www.leanprover.cn/install/>（实际上直接让 AI 为你安装即可）。

---

如果读者较为熟悉[类型论](@/posts/type_theory_1.md)，我们先指出：Lean 4 使用的是一种 CIC 的变体。

首先，宇宙层级定义如下，且是非累积的：

$$
\text{Prop} \coloneqq \operatorname{Sort} 0, \quad \operatorname{Type} u \coloneqq \operatorname{Sort} u + 1 \\\\
\operatorname{Sort} u : \operatorname{Sort} u + 1
$$

依赖函数的宇宙规则是：

$$
\frac{
    \Gamma \vdash A: \operatorname{Sort} u
    \qquad \Gamma, x : A \vdash B(x) : \operatorname{Sort} v
}{
    \Gamma \vdash \Pi (x : A).B(x): \mathrm{Sort} (\mathrm{imax}(u, v))
}
$$

$$
\operatorname{imax}(u, v) = \begin{cases}
   0 & v = 0 \\\\
   \max(u, v) & v > 0
\end{cases}
$$

对于 $P: \mathrm{Prop}$ 及 $p, q: P$，有 $p \equiv q$ 是定义相等，在内核中就忽略其差异。

另外，[$\eta$-归约](@/posts/lambda_calculus.md) $f \equiv \lambda x.\ f\ x$ 及积的 $p \equiv (p.1, p.2)$ 也是定义性相等。命题外延性由公理 `propext` 给出：

$$(P \leftrightarrow Q) \to P = Q$$

内核支持归纳族和互归纳定义，并为其提供 recursor 及相应的 $\iota$ 计算规则。递归函数最终被 elaboration 成 recursor 的使用。`Prop` 中的归纳类型只有满足某种条件时才能消去到非 `Prop` 的 `Sort`.

函数外延性可以通过商类型得到。商类型 `Quot` 是一个原始类型，配备有如下规则：

$$\mathrm{Quot.lift}\ f\ h\ (\mathrm{Quot.mk}\ r\ a) \equiv f\ a$$

公理 `Quot.sound` 额外提供了：

$$r\ a\ b \to \mathrm{Quot.mk}\ r\ a = \mathrm{Quot.mk}\ r\ b$$

Lean 通常使用的额外原则除了上述两者外，还有选择公理（这和外延性可以推出排中律）：

```lean
Classical.choice : Nonempty α → α
```

---

如果熟悉[形式化语言](@/posts/agda_1.md)的套路，易见以下含义：

```lean
def double : Nat → Nat
  | 0 => 0
  | n + 1 => double n + 2
```

以下例子展示了归纳法。这里 `ih` 是归纳假设；`show` 显示地指定当前目标的一个定义等价的形式，而后使用 `omega` 这个 tactic 自动完成整数的线性算术证明。

```lean
theorem double_eq (n : Nat) : double n = 2 * n := by
  induction n with
  | zero => rfl
  | succ k ih =>
    show double k + 2 = 2 * (k + 1)
    omega
```

日常 Lean 的使用风格不那么类型论，可以理解为维护一系列假设和目标（goal）。`by` 或 `=>` 之后同一缩进（或使用 `;`）的一行行命令可以理解为依次变换目标。列举基础 tactic 如下：

| 用法 | 效果 |
| :-: | :-: |
| `rfl` | / |
| `rw[h]` | 对 `h: a = b`，将目标中所有 `a` 换成 `b`；更复杂的 `h: ∀ a b, a + b = b + a` 等也有效 |
| `rw [← h]` | 将目标中所有 `b` 换成 `a` |
| `rw[h1, h2]` | 先用 `h1` 重写再用 `h2` 重写 |
| `rw [...] at h` | 在 `h` 上而不是目标上重写，`*` 表示在所有假设和目标上重写 |
| `simp[h1, h2]` | 反复使用规则简化直到无法简化；标注有 `@[simp]` 的引理会自动被使用，可以用 `simp only` 阻止此行为 |
| `exact h` | `h` 就是符合目标的东西；常使用 `trivial : True` |
| `apply h` | 形如 `exact` 但可以留洞，如 `apply Eq.trans` 留下了目标 `a = ?middle` 与目标 `?middle = c`，之后需要两次闭合目标 |
| `intro h` | 对 `P → Q` 将目标中的条件转化为假设，读者可参考[自然演绎系统](@/posts/logic_2.md)；对 `∀ x, P x` 引入一个任意的 `x` |
| `intro h1 h2` | 多次使用；使用时允许解构 |
| `have h : T := ...` | 在上下文中添加一个 `h : T`，但其值不透明 |
| `let x := ...` | 在上下文中添加一个定义，其值可见 |
| `suffices h : T by ...` | 反向的 `have` |
| `cases h with ...` | 通过归纳类型的构造器拆分目标 |
| `induction n with ...` | 通过归纳类型的归纳原则证明 |

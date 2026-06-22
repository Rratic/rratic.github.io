+++
title = "【范畴论】做习题 Universals and Limits"
draft = true

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "范畴论"]
+++

做一些 Mac Lane 书上 Ⅲ. Universals and Limits 的习题（从 2. The Yoneda Lemma 开始，前面的在[练习一](@/posts/category_theory_exercise_1.md)）。

我们回顾 Yoneda Lemma 是指，对范畴 $\mathcal{D}$ 有小 hom-sets 及其中对象 $r$ 与函子 $K: \mathcal{D} \to \mathbf{Set}$, 存在双射：

$$
\begin{aligned}
\Phi: \mathrm{Nat}(D(r, -), K) & \simeq Kr \cr
    \alpha & \mapsto \alpha_r(\mathbf{1}_r)
\end{aligned}
$$

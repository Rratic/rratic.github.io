+++
title = "【草稿】群论（一）：轨道"
date = 2025-06-09

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "笔记", "代数", "离散", "抽象", "抽象代数"]
+++

- [索引](/posts/index-group-theory/)

## 群在集合上的作用
群 $G$ 在集合 $\Omega$ 上的作用是一个映射

$$
\begin{aligned}
\varphi \colon &G \to (\Omega\to\Omega),\\\\
        &x \mapsto (\alpha\mapsto\alpha^x)
\end{aligned}
$$

满足单位元对应恒等映射，且 $(\alpha^x)^y = \alpha^{xy}$

你或许可以找到不同形式，如

$$
\begin{aligned}
f \colon &G\times S \to S,\\\\
        &(g, s) \mapsto g(s)
\end{aligned}
$$

但前一种更直观一些。

显然有 $\varphi$ 是从 $G$ 到 $\Omega$ 上变换群的同态。

记集合元素 $\alpha$ 在这一关系下的等价类 $\\{\alpha^x|x\in G\\}$ 为其轨道 $Orb(\alpha)$；全体不变映射 $\\{x|\alpha^x=\alpha\\}$ 为其稳定子群 $Stab(\alpha)$

易知 $|Orb(\alpha)|=|G\colon Stab(\alpha)|$

例如，对于正四面体（记顶点 $\\{A,B,C,D\\}$），设其旋转变换群为 $G$，则：任取一个顶点，它对应的稳定子群阶为 3，轨道为 $\\{A,B,C,D\\}$，故而 $G$ 是 $S_4$ 的 12 阶子群，必然是 $A_4\cong V_4\oplus Z_3$

## Sylow 定理

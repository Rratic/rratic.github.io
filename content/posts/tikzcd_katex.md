+++
title = "给 KaTeX 添加 Tikzcd 支持"
date = 2026-11-18

[extra]
math = true
tikzcd = true

[taxonomies]
categories = ["项目"]
tags = ["数学", "计算机"]
+++

项目仓库见于 [Tikzcd-KaTeX](https://github.com/Rratic/tikzcd-katex)，使用示例如下：

```tex
\begin{tikzcd}
	X \arrow[r, "f"] \arrow[d, two heads] & Y \\
	\mathrm{coim}(f) \arrow[r, dashed] & \mathrm{im}(f) \arrow[u, hook]
\end{tikzcd}
```

$$
\begin{tikzcd}
	X \arrow[r, "f"] \arrow[d, two heads] & Y \\\\
	\mathrm{coim}(f) \arrow[r, dashed] & \mathrm{im}(f) \arrow[u, hook]
\end{tikzcd}
$$

```tex
\begin{tikzcd}
	Y \arrow[rd, "\phi" description, dashed]
	\arrow[rrd, "\varphi_1"]
	\arrow[rdd, "\varphi_2"']
	&& \\
	& P \arrow[r, "\pi_1"'] \arrow[d, "\pi_2"] & {X_1} \\
	& {X_2}
\end{tikzcd}
```

$$
\begin{tikzcd}
	Y \arrow[rd, "\phi" description, dashed]
	\arrow[rrd, "\varphi_1"]
	\arrow[rdd, "\varphi_2"']
	&& \\\\
	& P \arrow[r, "\pi_1"'] \arrow[d, "\pi_2"] & {X_1} \\\\
	& {X_2}
\end{tikzcd}
$$

```tex
\begin{tikzcd}
	A \arrow[r] \arrow[d] \arrow[dr, phantom, "\ulcorner", very near start]
	& B \arrow[d] \\
	C \arrow[r] & D
\end{tikzcd}
```

$$
\begin{tikzcd}
	A \arrow[r] \arrow[d] \arrow[dr, phantom, "\ulcorner", very near start]
	& B \arrow[d] \\\\
	C \arrow[r] & D
\end{tikzcd}
$$

```tex
\begin{tikzcd}
	& \pi_1(U_1) \arrow[dr] \arrow[drr,"j_1"] && \\
	\pi_1(U_1\cap U_2) \arrow[ur, "i_1"] \arrow[dr,"i_2" swap] &&
	\pi_1(U_1) \ast_{\pi_1(U_1\cap U_2)} \pi_1(U_2) \arrow[r, dashed, "\simeq"] & \pi_1(X) \\
	& \pi_1(U_2) \arrow[ur] \arrow[urr, "j_2" swap]
\end{tikzcd}
```

$$
\begin{tikzcd}
	& \pi_1(U_1) \arrow[dr] \arrow[drr,"j_1"] && \\\\
	\pi_1(U_1\cap U_2) \arrow[ur, "i_1"] \arrow[dr,"i_2" swap] &&
	\pi_1(U_1) \ast_{\pi_1(U_1\cap U_2)} \pi_1(U_2) \arrow[r, dashed, "\simeq"] & \pi_1(X) \\\\
	& \pi_1(U_2) \arrow[ur] \arrow[urr, "j_2" swap]
\end{tikzcd}
$$

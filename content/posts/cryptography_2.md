+++
title = "【密码学】经典数论算法：基于有限域上离散对数/整数分解/二次剩余"
date = 2026-02-12

[extra]
toc = true
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "密码学"]
+++

主要参考的是 An Introduction to Mathematical Cryptography 第二、三、四章。

<!-- more -->

使用的是 Rust, 读者可在 [Rust Playground](https://play.rust-lang.org/) 中运行。

## 离散对数
{% admonition(type="question", title="离散对数问题") %}
已知素数 $p$ 与与之互素的整数 $g, h$，已知存在 $g^x \equiv h \pmod p$，求 $x$.
{% end %}

当前解决一般的离散对数问题的最优算法是 $O(\sqrt{p})$ 的。

更广义的离散对数问题是在一般的群上考虑这个问题（这里是 $\mathbb{F}_p^\times$）。

---

Diffie–Hellman Key Exchange 用于解决这样的困境：Alice 与 Bob 两人希望共有一个用于对称加密的密钥，但是他们间的交流方式是不安全的。

{% admonition(type="tip", title="Diffie–Hellman Key Exchange") %}
流程如下：
1. 选取一个大素数 $p$ 和整数 $g$ 在 mod $p$ 意义下有较高的阶
2. Alice 与 Bob 分别秘密地选取整数 $a, b$ 并在 mod $p$ 意义下计算 $A \equiv g^a, B \equiv g^b$
3. Alice 与 Bob 分别告知对方 $B, A$ 的值
4. 现在他们共有一个密钥 $K \equiv g^{ab}$
{% end %}

其中只用到最基础的 $O(\ln p)$ 快速幂算法：

```rs
use num::traits::{One, Zero};
use std::ops::{BitAnd, Mul, Rem, ShrAssign};

// does not do `n %= (p-1)`, assume n >= 0
fn mod_pow<T, E>(a: T, mut n: E, p: T) -> T
where
    T: Copy + One + Mul<Output = T> + Rem<Output = T>,
    E: Copy + Zero + One + ShrAssign<u8> + BitAnd<Output = E> + PartialEq,
{
    let mut result = T::one();
    let mut base = a % p;

    let one = E::one();

    while !E::is_zero(&n) {
        if E::is_one(&(n & one)) {
            result = (result * base) % p;
        }
        n >>= 1;
        base = (base * base) % p;
    }

    result
}
```

---

Diffie–Hellman Key Exchange 算法并不是一个完整的公钥密码体系。紧随此的最自然的密码体系是 Elgamal Public Key Cryptosystem.

{% admonition(type="tip", title="Elgamal Public Key Cryptosystem") %}
创建公钥过程如下：
1. 选取一个大素数 $p$ 和整数 $g$ 在 mod $p$ 意义下有较高的阶
2. Alice 秘密地选取整数 $a$ 并在 mod $p$ 意义下计算 $A \equiv g^a$
3. Alice 公布 $A$

Bob 对原文 $m$ 加密过程如下：
1. 秘密地选取整数 $k$
2. 在 mod $p$ 意义下计算 $c_1 \equiv g^k, c_2 \equiv mA^k$
3. 将 $(c_1, c_2)$ 发给 Alice

Alice 解密过程即在 mod $p$ 意义下计算 $(c_1^a)^{-1} \cdot c_2$.
{% end %}

这里涉及 mod $p$ 意义下求逆，这并没有什么特别良好的算法，可以考虑计算 $a^{p-2}$ 或使用扩展 Euclid 算法。

扩展 Euclid 算法如下：

```rs
use num::traits::{One, Zero};
use std::ops::{Div, Mul, Rem, Sub};

// ext_gcd(a, b) = (d, x, y) where ax + by = d
fn ext_gcd<T>(a: T, b: T) -> (T, T, T)
where
    T: Copy
        + Zero
        + One
        + Rem<Output = T>
        + Div<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + PartialEq,
{
    if T::is_zero(&b) {
        return (a, T::one(), T::zero());
    }

    let (res, x1, y1) = ext_gcd(b, a % b);

    (res, y1, x1 - (a / b) * y1)
}
```

对固定的 $g$ 与 $p$, 若 Diffie–Hellman 问题可被某种算法攻破，则 Elgamal PKC 亦可；反之亦然。这一点读者自证不难。

---

现在来看一些处理离散对数问题的算法：

{% admonition(type="tip", title="Babystep–Giantstep Algorithm") %}
对群 $G$ 中阶为 $N$ 的元素 $g$ 和另一元素 $h$，可用以下方法求得 $g^x = h$ 的解：
1. 取 $n$ 满足 $n > \sqrt{N}$
2. 建立列表 $e, g, g^2, \cdots, g^n$ 与 $h, hg^{-n}, \cdots, hg^{-n^2}$
3. 找到其中相等的 $g^i = hg^{-jn}$，就有 $x = i + jn$

其时间复杂度 $O(\sqrt{N} \cdot \ln N)$，空间复杂度 $O(\sqrt{N})$.
{% end %}

读者易证其正确性。时间复杂度是来源于找到相等的项，这可以通过排序实现。

对 $G = \mathbb{F}_p^\times$，由 Lagrange 定理知 $N \mid p-1$，这可以用于推断 $N$ 的值。

{% admonition(type="tip", title="Pohlig–Hellman Algorithm") %}
对群 $G$ 若我们有一个算法 `oracle` 使得对阶是 $q^k, q \in \mathbb{P}$ 的元素可以以 $O(S_{q^k})$ 解决离散对数问题，则对一般的元素 $g$ 满足阶为：

$$N = q_1^{k_1} \cdots q_t^{k_t}$$

存在时间复杂度为：

$$O\left(\sum S_{q_i^{k_i}} + \ln N\right)$$

的算法如下：
1. 对每个 $i$ 计算 $g_i = g^{N/q_i^{k_i}}, h_i = h^{N/q_i^{k_i}}$
2. 调用 `oracle` 解 $g_i^{y_i} = h_i$
3. 使用中国剩余定理解 $x \equiv y_i \pmod {q_i^{k_i}}$
{% end %}

## 整数分解
{% admonition(type="tip", title="RSA Public Key Cryptosystem") %}
创建公钥过程如下：
1. Alice 秘密地选取大素数 $p, q$ 及整数 $e$ 满足 $\gcd(e, (p-1)(q-1)) = 1$
2. Alice 公布 $N = pq$ 与 $e$

Bob 对原文 $m$ 加密过程即计算 $c \equiv m^e \pmod N$.

Alice 解密过程为：
1. 计算 $d$ 使得 $de \equiv 1 \pmod {(p-1)(q-1)}$
2. 求 $m' \equiv c^d \pmod N$ 即为结果
{% end %}

这是一个使用广泛的标准（标准文档是 RFC 8017, 标题为 PKCS #1: RSA Cryptography Specifications Version 2.2），如 2018 以前互联网使用的 TLS 1.2 是用 RSA 来交换用于生成 AES 密钥的密钥。当前推荐使用的最低要求是 RSA-2048, 即使用 2048 位密钥（可参考 RSA Factoring Challenge 的状态）。尽管现在已逐步推荐向椭圆曲线密码迁移。

容易说明根据 $N$ 找到 $(p-1)(q-1)$ 的难度与找到 $N$ 的分解的难度是相同的。

---

现在来考虑中间人攻击。对 Diffie–Hellman Key Exchange 来说，中间人攻击是这样的：在 Alice 与 Bob 通过信道告知对方 $B, A$ 的值时，Eve 在中间将消息篡改成一个自己知道的 $g^e$. 这样一来，后续的消息可以被窃取到（如果仍然可以篡改消息，则这一行为不会被发现）。

对 RSA PKC 来说，会出现这样的情况：
1. 若 Bob 发送了密文 $c$，则 Eve 考虑一个随机的 $k$ 计算 $c' \equiv k^e \cdot c$，然后要求 Alice 给出其解（理由可能是通过解一个随机的密文来证明身份）。给出的解 $m' \equiv (k^e \cdot c)^d \equiv k \cdot m$ 可推断出原文 $m$.
2. Alice 对同一个 $N$ 创建了多个 $e$，如 $e_1, \cdots, e_r$，并且 Bob 均使用了它们，发送 $c_i \equiv m^{e_i}$. 若 $\gcd(e_1, \cdots, e_r) = 1$，则 Eve 可直接计算出 $m$.

---

为了进行 RSA PKC，实际上我们还需要能够判断一个大数是否是素数。

一个想法是考虑是否对所有 $a$ 都成立 $a^n \equiv a \pmod n$. 但实际上这样的 $n$ 不一定是素数，反例如 561.

{% admonition(type="tip", title="Miller–Rabin Test") %}
对整数 $n$ 使用 $a$ 作素性测试过程如下：
1. 排除 $n$ 偶，及 $1 < \gcd(a, n) < n$ 的平凡情况
2. 写成 $n - 1 = 2^kq$，其中 $q$ 为奇
3. 如果 $a^q \not \equiv 1 \pmod n$，且对 $0 \leq i < k$ 皆有 $a^{2^i\cdot q} \not\equiv -1 \pmod n$，则 $n$ 是合数
{% end %}

其正确性易见。

存在以下事实：
1. 对一个奇合数 $n$，在 $1 \sim n-1$ 间至少 75% 的 $a$ 选择可以成功用作 Miller–Rabin 测试，因此一般人们会考虑对随机的 50 ~ 100 个数作测试来得到结论
2. 若广义黎曼假设成立，则对每个奇合数 $n$ 都存在 $a \leq 2(\ln n)^2$ 可以成功用作 Miller–Rabin 测试
3. 对任意 $\epsilon > 0$ 都存在一个算法在 $O((\ln n)^{6+\epsilon})$ 步中判定 $n$ 是否是素数，称为 AKS Test

---

现在考虑从 $N$ 分解出 $p$ 的算法。

如果存在一个 $L$ 使得 $p-1 \mid L, q-1 \nmid L$，则有 $p = \gcd(a^L-1, N)$. 依此 Pollard 的观察是：如果 $p-1$ 可以分解成小素数的乘积，那么可以考虑取 $L = n!$.

{% admonition(type="tip", title="Pollard’s p − 1 Factorization Algorithm") %}
分解整数 $N$ 的流程如下：
1. 令 $a = 2$ 或某个好算的值
2. 考虑 $j = 2, 3, \cdots$ 至某个特定的界，计算 $d = \gcd(a^{j!}-1, N)$，如果 $1 < d < N$ 则 $d$ 是一个因子

为了增加效率，可以每次隔 $k$ 个再计算 $d$.
{% end %}

这给出的启示是：即使建立好了一个看起来很好的加密系统，也需要注意它在特殊情形下可能很容易解决。

我们称一个数 $n \mid B!$ 是一个 $B$-smooth number. 记 $\psi(X, B)$ 是满足 $1 < n \leq X$ 的 $B$-smooth number 个数。一个结论是说，对 $0 < \epsilon < \frac{1}{2}$，令 $u = \frac{\ln X}{\ln B}$，在 $(\ln X)^\epsilon < \ln B < (\ln X)^{1-\epsilon}$ 时 $\psi(X, B) = X \cdot u^{-u(1+o(1))}$.

---

另一个想法是：如果可以写出 $N + b^2 = a^2$，那么 $(a + b)(a - b)$ 可以用于分解。而在 $k$ 较小时，$kN + b^2 = a^2$ 也是有用的。

这给出了如下现代算法：
1. 找到若干整数 $a_1, \cdots, a_r$ 使得有 $c_i \equiv a_i^2 \pmod N$ 可以被分解为小素数乘积
2. 取一个 $c_{i_1} \cdots c_{i_s}$ 使它是平方数 $b^2$
3. 令 $a = a_{i_1} \cdots a_{i_s}$，则 $a^2 \equiv b^2 \pmod N$，故而 $\gcd(a-b, N)$ 很有可能是非平凡因子

这里的第二步实际上是一个 $\mathbb{F}_2$ 上的线性方程问题，由于是稀疏的，有较好的算法。

这里的第一步需要尽量有效地找到足够多满足 $a_i > \sqrt{N}$ 且对应的 $c_i$ 是 $B$-smooth 的算法。对 $N < 2^{350}$ 来说最有效的是二次筛法，而对 $N > 2^{450}$ 目前最有效的是数域筛法。

---

我们希望找到接近 $\sqrt{kN}$ 的分数 $\frac{a}{b}$, 这可以用连分数来完成

这里给出 Pomerance 的二次筛法的简单版本：

先找到许多小于某个界 $X$ 的 $B$-smooth number. 我们可以改造 Eratosthenes 筛，改为每次作除法，看最后哪些值变为 1. 实际上需要的是找到其中形如 $a^2 \pmod N$ 的那些。因此考虑多项式 $F(T) = T^2 - N$ 并取 $a = \lfloor \sqrt{n} \rfloor + 1$, 对一列数 $F(a), F(a+1), \cdots, F(b)$ 使用前述筛法。

---

现在介绍数域筛法的大致思路：

我们先找到非零整数 $m$ 与首一的不可约整系数多项式 $f$ 满足 $f(m) \equiv 0 \pmod N$. 例如对 $N = 2^{2^9} + 1$ 考虑 $m = 2^{103}, f(x) = x^5 + 8$. 记 $\beta$ 是一个根，之后的工作将在 $\mathbb{Z}[\beta] \simeq \mathbb{Z}[x] / (f(x))$ 上进行。

接下来是希望找大量的整数对 $(a_1, b_1), \cdots, (a_k, b_k)$ 满足：

$$\prod_{i=1}^k (a_i - b_im) = A^2,\quad \exists A \in \mathbb{Z}$$

$$\prod_{i=1}^k (a_i - b_i\beta) = \alpha^2,\quad \exists \alpha \in \mathbb{Z}[\beta]$$

设 $\alpha = c_0 + c_1\beta + \cdots c_{d-1}\beta^{d-1}$. 由于 $m \equiv \beta \pmod N$ 我们有 $A^2 \equiv \alpha^2 \pmod N$. 另一方面 $\alpha \equiv (B := c_0 + c_1m + \cdots c_{d-1}m^{d-1}) \pmod N$, $\gcd(A - B, N)$ 很有可能是非平凡因子。

---

对于 $\mathbb{F}_p$ 上的离散对数问题，有一个算法 Index Calculus 是这样的：

选择一个数 $B$, 转而求解 $g^x \equiv l \pmod p, \ \forall l \leq B, l \in \mathbb{P}$. 这样以后，依次对 $k = 1, 2, \cdots$ 枚举 $h \cdot g^{-k} \pmod p$ 直到找到一个 $B$-smooth 的，我们有：

$$h \cdot g^{-k} \equiv \prod_{l \leq B} l^{e_l} \pmod p$$

这样一来可以把离散对数的结果写成：

$$\log_g(h) \equiv k + \sum_{l\leq B} e_l \cdot \log_g(l) \pmod {p-1}$$

现在回看对于小素数 $l$ 如何找到 $\log_g(l)$. 我们随机取一些 $0 < g_i < p$ 计算 $g_i \equiv g^i \pmod p$, 舍弃其中不 $B$-smooth 的。对 $B$-smooth 的 $g_i$ 有：

$$g_i = \prod_{l\leq B} l^{u_l(i)}$$

$$i \equiv \sum_{l\leq B} u_l(i) \cdot \log_g(l) \pmod {p-1}$$

这样一来就可以用线性代数方法解决。

---

现在考虑二次剩余。假定读者已知晓对应的初等数论内容：记 Legendre 符号：

$$
\left(\frac{a}{p}\right) = \begin{cases}
0 & p\mid a \\\\
1 & a \equiv c^2 \pmod p \\\\
-1 & \text{else}
\end{cases}
$$

从中定义 Jacobi 符号：

$$\left(\frac{a}{p_1^{e_1} \cdots p_t^{e_t}}\right) = \left(\frac{a}{p_1}\right)^{e_1} \cdots \left(\frac{a}{p_t}\right)^{e_t}$$

除基本性质外，对奇素数 $a, b$ 成立：

$$\left(\frac{-1}{b}\right) = \begin{cases} 1 & b \equiv 1 \pmod 4 \\\\ -1 & b \equiv 3 \pmod 4 \end{cases}$$

$$\left(\frac{2}{b}\right) = \begin{cases} 1 & b \equiv 1, 7 \pmod 8 \\\\ -1 & b \equiv 3, 5 \pmod 8 \end{cases}$$

$$\left(\frac{a}{b}\right)\left(\frac{b}{a}\right) = \begin{cases} 1 & \text{else} \\\\ -1 & a, b \equiv 3 \pmod 4 \end{cases}$$

这样一来我们可以对任意两整数使用辗转相除求值。

使用二次剩余可以给出：

{% admonition(type="tip", title="Goldwasser–Micali Probabilistic Public Key Cryptosystem") %}
创建公钥过程如下：
1. Alice 秘密地选取大素数 $p, q$ 及整数 $a$ 满足 $(\frac{a}{p}) = (\frac{a}{q}) = -1$
2. Alice 公布 $N = pq$ 与 $a$

Bob 对原文 $m \in \\{0, 1\\}$ 加密过程如下：
1. 随机取 $1 < r < N$
2. 若 $m = 0$ 计算 $c \equiv r^2 \pmod N$, 否则计算 $c \equiv ar^2 \pmod N$
3. 将 $c$ 发给 Alice

Alice 解密过程即计算 $(\frac{c}{p})$.
{% end %}

这个算法并不实用，因为会导致信息大小的大约 $\log_2(N)$, 至少千倍的膨胀。

## 数字签名
数字签名考虑的是一个不同的，在数字时代与 PKC 同等重要的问题。现在有一个文件 $D$，而 Susan 希望使用私钥创建一个额外的信息 $D^\text{Sus}$ 用来表达自己签字认可该文件，并且之后可以用一个公钥和验证算法来检验。

一般来说，数字签名方案是作用于小的数据大小，如 80~100 bits. 因此对一般的文件需要先经过密码学安全的 hash. 本文中略去。关于盲签名，本文中也略去。

{% admonition(type="tip", title="RSA Digital Signatures") %}
创建公钥过程与 RSA PKC 相同。

签名时，Susan 取 $d$ 使 $de \equiv 1 \pmod {(p-1)(q-1)}$，计算 $S \equiv D^d \pmod N$ 为签名。

验证时只需验证 $S^e \equiv D \pmod N$.
{% end %}

实际上为了增加效率 $d$ 可以只满足：

$$de \equiv 1 \pmod {\frac{(p-1)(q-1)}{\gcd(p-1, q-1)}}$$

{% admonition(type="tip", title="Elgamal Digital Signature Algorithm") %}
创建公钥过程如下：
1. 选取一个大素数 $p$ 和整数 $g$ 在 mod $p$ 意义下有较高的阶
2. Susan 秘密地选取整数 $a$ 并在 mod $p$ 意义下计算 $A \equiv g^a$
3. Susan 公布 $A$

Susan 对文件 $D$ 签名过程如下：
1. 秘密地选取整数 $k$ 满足 $\gcd(k, p-1) = 1$
2. 计算 $S_1 \equiv g^k \pmod p$ 及 $S_2 \equiv (D - aS_1)k^{-1} \pmod {p-1}$
3. $(S_1, S_2)$ 即是签名

验证时只需验证 $A^{S_1}S_1^{S_2} \equiv g^D \pmod p$.
{% end %}

这里 $S_1$ 与 $S_2$ 是对不同的数取模，但 $S_2$ 是基于一个确定的 $S_1$ 值，读者可验证正确性。

{% admonition(type="tip", title="The digital signature algorithm (DSA)") %}
创建公钥过程如下：
1. 选取一个大素数 $p, q$ 满足 $p \equiv 1 \pmod q$
2. 选取整数 $g$ 在 mod $p$ 下阶为 $q$
3. Susan 秘密地选取整数 $a$ 并在 mod $p$ 意义下计算 $A \equiv g^a$
4. Susan 公布 $A$

Susan 对文件 $D \pmod q$ 签名过程如下：
1. 秘密地选取整数 $k$ 满足 $1 < k < q$
2. 计算 $S_1 \equiv (g^k \pmod p) \pmod q$ 及 $S_2 \equiv (D + aS_1)k^{-1} \pmod q$
3. $(S_1, S_2)$ 即是签名

验证过程如下：
1. 计算 $V_1 \equiv DS_2^{-1} \pmod q$ 及 $V_2 \equiv S_1S_2^{-1} \pmod q$
2. 验证 $(g^{V_1}A^{V_2} \pmod p) \pmod q = S_1$
{% end %}

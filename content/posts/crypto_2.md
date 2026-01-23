+++
title = "密码学（二）：经典数论算法"
draft = true

[extra]
toc = true
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "密码学"]
+++

主要参考的是 An Introduction to Mathematical Cryptography 第二、三章。

使用的是 Rust, 读者可在 [Rust Playground](https://play.rust-lang.org/) 中运行。

## 离散对数
{% admonition(type="question", title="离散对数问题") %}
已知素数 $p$ 与与之互素的整数 $g, h$，已知存在 $g^x \equiv h \mod p$，求 $x$.
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
3. 使用中国剩余定理解 $x \equiv y_i \mod q_i^{k_i}$
{% end %}

## 整数分解
{% admonition(type="tip", title="RSA Public Key Cryptosystem") %}
创建公钥过程如下：
1. Alice 秘密地选取大素数 $p, q$ 及整数 $e$ 满足 $\gcd(e, (p-1)(q-1)) = 1$
2. Alice 公布 $N = pq$ 与 $e$

Bob 对原文 $m$ 加密过程即计算 $c \equiv m^e \mod N$.

Alice 解密过程为：
1. 计算 $d$ 使得 $ed \equiv 1 \mod (p-1)(q-1)$
2. 求 $m' \equiv c^d \mod N$ 即为结果
{% end %}

容易说明根据 $N$ 找到 $(p-1)(q-1)$ 的难度与找到 $N$ 的分解的难度是相同的。

---

现在来考虑中间人攻击。对 Diffie–Hellman Key Exchange 来说，中间人攻击是这样的：在 Alice 与 Bob 通过信道告知对方 $B, A$ 的值时，Eve 在中间将消息篡改成一个自己知道的 $g^e$. 这样一来，后续的消息可以被窃取到（如果仍然可以篡改消息，则这一行为不会被发现）。

对 RSA PKC 来说，会出现这样的情况：
1. 若 Bob 发送了密文 $c$，则 Eve 考虑一个随机的 $k$ 计算 $c' \equiv k^e \cdot c$，然后要求 Alice 给出其解（理由可能是通过解一个随机的密文来证明身份）。给出的解 $m' \equiv (k^e \cdot c)^d \equiv k \cdot m$ 可推断出原文 $m$.
2. Alice 对同一个 $N$ 创建了多个 $e$，如 $e_1, \cdots, e_r$，并且 Bob 均使用了它们，发送 $c_i \equiv m^{e_i}$. 若 $\gcd(e_1, \cdots, e_r) = 1$，则 Eve 可直接计算出 $m$.

---

为了进行 RSA PKC，实际上我们还需要能够判断一个大数是否是素数。

一个想法是考虑是否对所有 $a$ 都成立 $a^n \equiv a \mod n$. 但实际上这样的 $n$ 不一定是素数，反例如 561.

{% admonition(type="tip", title="Miller–Rabin Test") %}
对整数 $n$ 使用 $a$ 作素性测试过程如下：
1. 排除 $n$ 偶，及 $1 < \gcd(a, n) < n$ 的平凡情况
2. 写成 $n - 1 = 2^kq$，其中 $q$ 为奇
3. 如果 $a^q \not \equiv 1 \mod n$，且对 $0 \leq i < k$ 皆有 $a^{2^i\cdot q} \not\equiv -1 \mod n$，则 $n$ 是合数
{% end %}

其正确性易见。

有一个好的地方是：对一个偶合数 $n$，在 $1 \sim n-1$ 间至少 75% 的 $a$ 选择可以成功用作 Miller–Rabin 测试。

{{ todo() }}

## 数字签名

{{ todo() }}

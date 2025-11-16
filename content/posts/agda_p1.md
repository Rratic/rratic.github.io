+++
title = "Agda 学习（一）"
date = 2025-11-12

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "函数式编程", "Agda"]
+++

前置知识
- [Haskell](/posts/haskell-p1/)

参考阅读：
- *Verified Functional Programming in Agda*

## 环境配置
参考 <https://agda.readthedocs.io/en/latest/getting-started/installation.html> 的指引

Agda 是用 Haskell 写的，可以在[安装好 Haskell](/posts/haskell-p1/) 后通过如下命令安装：
```cmd
cabal update
cabal install Agda
```

在 Windows 下，如果 Agda 程序路径没有被自动添加到 PATH 下，可手动在命令行运行（后面的路径是 Cabal 安装路径）：
```cmd
setx PATH "%PATH%;D:\cabal\bin"
```

之后的步骤请参考 <https://higher-order.fun/cn/2023/11/15/InstallAgda.html>

## 从布尔开始
在 2.8 标准库中，布尔类型的定义如下
```agda
module Agda.Builtin.Bool where

data Bool : Set where
  false true : Bool
```

有关的定义如：
```agda
infix 0 if_then_else_

if_then_else_ : Bool → A → A → A
if true  then t else f = t
if false then t else f = f
```

这些语法与 Haskell 非常相似。

## 证明
Curry-Howard 同构的想法是：类型是命题，而实例是类型的证明。

`refl` 的原型如下：
```agda
module Agda.Builtin.Equality where

infix 4 _≡_
data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  instance refl : x ≡ x
```

可以以如下方式使用：
```agda
~~tt : ~ ~ true ≡ true
~~tt = refl{lzero}{Bool}{true}
~~ff : ~ ~ false ≡ false
~~ff = refl{lzero}{Bool}{false}
```

可以被简化为 `~~tt = refl`.

一个更一般的命题及其证明如下：
```agda
~~-elim : ∀ (b : Bool) → ~ ~ b ≡ b
~~-elim true = refl
~~-elim false = refl
```

或者使用
```agda
~~-elim : ∀ (b : Bool) → ~ ~ b ≡ b
~~-elim true = ~~tt
~~-elim false = ~~ff
```

对于带假设的证明，可使用 `()` 表示条件不可能。
```agda
||≡ff : ∀ {b1 b2} → b1 || b2 ≡ false → b2 ≡ false
||≡ff {true} ()
||≡ff {false}{true} ()
||≡ff {false}{false} p = refl
```

对于等式，可以使用重写关键字 `rewrite`.
```agda
||-cong : ∀ {b1 b2 b2'} → b2 ≡ b2' → b1 || b2 ≡ b1 || b2'
||-cong p rewrite p = refl
```

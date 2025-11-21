+++
title = "【草稿】Agda 学习（一）"
description = "类型论的实践。"
date = 2025-11-12
updated = 2025-11-21

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

参考的是[北京大学 - 计算概论A实验班 函数式程序设计 2025秋](https://zhenjiang888.github.io/FP/2025/)下半学期讲义。

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

接下来需要安装标准库，步骤请参考 <https://higher-order.fun/cn/2023/11/15/InstallAgda.html>

在 VSCode 的 `agda-mode` 插件中，打开一个 Agda 文件，按 `Ctrl + C` 再按 `Ctrl + L` 来加载它。

## 概览
### 布尔类型
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

### 使用 `refl` 证明
Curry-Howard 同构的想法是：类型是命题，而实例是类型的证明。

`refl` 的原型如下：
```agda
module Agda.Builtin.Equality where

infix 4 _≡_
data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  instance refl : x ≡ x
```

这里的 `{}` 表示隐式参数，可以被自动推断；而 `()` 表示显式参数；是否加 `∀` 只是风格问题。

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

对于等式，可以使用重写关键字 `rewrite`，其后接的是一个 `left ≡ right` 型的命题，这将会把表达式表层的 `left`（模式匹配）变为 `right`.
```agda
||-cong : ∀ {b1 b2 b2'} → b2 ≡ b2' → b1 || b2 ≡ b1 || b2'
||-cong p rewrite p = refl
```

## 归纳
### 自然数类型
自然数类型的定义如下：
```agda
data Nat : Set where
  zero : Nat
  suc  : (n : Nat) → Nat
```

各种运算符都是归纳定义的。

我们来看加法结合律的证明：
```agda
+assoc : ∀ (x y z : ℕ) → x + (y + z) ≡ (x + y) + z
+assoc zero y z = refl
+assoc (suc x) y z rewrite +assoc x y z = refl
```

我们给一个小的定理
```agda
+0 : ∀ (x : ℕ) → x + 0 ≡ x
+0 zero = refl
+0 (suc x) rewrite +0 x = refl
```

### 使用 `hole`
可以通过 `hole` 来交互式地得到以上证明，流程如下：
```agda
-- 引入 hole
+assoc zero y z = ?
-- 按 Ctrl + C 再按 Ctrl + L
+assoc zero y z = {! 0!}
-- 显示 Goal: y+z ≡ y+z
```

### 辅助引理
为了证明 `+comm : ∀(x y : N) → x+y ≡ y+x`，分为两步
```agda
+comm zero y rewrite +0 y = refl
+comm (suc x) y rewrite +comm x y = ?
```

这里的 `hole` 是 `suc (y + x) ≡ y + suc x`，额外用一个引理证明它：
```agda
+suc : ∀ (x y : N) → x + (suc y) ≡ suc (x + y) 
+suc zero y = refl 
+suc (suc x) y rewrite +suc x y = refl
```

现在就可用 `+comm (suc x) y rewrite +suc y x | +comm x y = refl` 来完成。

对于更复杂的命题如 `*distribr : ∀ (x y z : N) → (x + y) * z ≡ x * z + y * z`，需选取合适的归纳变量。

在这个结论中
```agda
<-trans : ∀ {x y z : ℕ} → x < y ≡ true → y < z ≡ true → x < z ≡ true
<-trans p q = ?
```

加载将会显示
```agda
Goal: .x < .z ≡ true
------------------------------------------------
p : .x < .y ≡ true
q : .y < .z ≡ true
.z : N
.y : N 
.x : N
```

### 相等测试
一个 `A → A → Bool` 类型的函数被称为相等测试，如果它返回 `true` 的条件是 `x ≡ y` 可证。

```agda
=ℕ_ : ℕ → ℕ → Bool
0 =ℕ 0 = true
suc x =ℕ suc y = x =ℕ y
_ =ℕ _ = false
```

### 相互递归
```agda
is-even zero = true
is-even (suc x) = is-odd x
is-odd zero = false
is-odd (suc x) = is-even x
```

### 列表
定义如下：
```agda
infixr 5 _∷_
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A
```

### 例子
现在来看一个大例子：自然数乘法交换律的证明。
```agda
module Main where

open import Data.Nat using (ℕ; zero; suc; _+_; _*_)
open import Data.Bool using (Bool; true; false; _∨_; if_then_else_)
open import Data.Vec using (Vec; []; _∷_)
open import Data.List using (List; []; _∷_)

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; sym; trans; cong; cong-app)
open Eq.≡-Reasoning using (begin_; step-≡-⟩; step-≡-⟨; step-≡-∣; _∎)

+0 : (x : ℕ) → x + 0 ≡ x
+0 zero = refl
+0 (suc x) rewrite +0 x = refl

+suc : (x y : ℕ) → x + (suc y) ≡ suc (x + y) 
+suc zero y = refl
+suc (suc x) y rewrite +suc x y = refl

+-comm : (x y : ℕ) → x + y ≡ y + x
+-comm zero y rewrite +0 y = refl
+-comm (suc x) y rewrite +suc y x | +-comm x y = refl

+-assoc : (x y z : ℕ) → x + (y + z) ≡ (x + y) + z
+-assoc zero y z = refl
+-assoc (suc x) y z rewrite +-assoc x y z = refl

*0 : (x : ℕ) → x * 0 ≡ 0
*0 zero = refl
*0 (suc x) rewrite *0 x = refl

*-mylemmalemma : (x y z : ℕ) → y + (x + z) ≡ x + (y + z)
*-mylemmalemma x y z rewrite +-assoc y x z | cong (_+ z) (+-comm y x) | +-assoc x y z = refl

*-mylemma : (x y : ℕ) → y + y * x ≡ y * suc x
*-mylemma x zero = refl
*-mylemma x (suc y) rewrite *-mylemmalemma x y (y * x) | cong suc (cong (x +_) (*-mylemma x y)) = refl

*-comm : (x y : ℕ) → x * y ≡ y * x
*-comm zero y rewrite *0 y = refl
*-comm (suc x) y rewrite *-comm x y = *-mylemma x y
```

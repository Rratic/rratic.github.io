+++
title = "Agda 学习（一）：完成简单证明的一切"
description = "类型论的实践：函数式程序推理与演算。Agda 中的证明、归纳、列表、Internal Verification 和等式理论。"
date = 2025-11-12
updated = 2025-12-13

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "函数式编程"]
+++

{{ ref_index(to = "functional-programming") }}

前置知识
- [Haskell](/posts/haskell-1/)

参考的是[北京大学 - 计算概论A实验班 函数式程序设计 2025秋](https://zhenjiang888.github.io/FP/2025/)下半学期讲义。

参考阅读：
- *Verified Functional Programming in Agda*

## 环境配置
可参考 <https://agda.readthedocs.io/en/latest/getting-started/installation.html> 的指引。

Agda 是用 Haskell 写的，可以在[安装好 Haskell](/posts/haskell-1/) 后通过如下命令安装：
```cmd
cabal update
cabal install Agda
```

在 Windows 下，如果 Agda 程序路径没有被自动添加到 PATH 下，可手动在命令行运行（后面的路径是 Cabal 安装路径）：
```cmd
setx PATH "%PATH%;D:\cabal\bin"
```

接下来需要安装标准库，步骤请参考 [配置 Agda 开发环境（2023）](https://higher-order.fun/cn/2023/11/15/InstallAgda.html)，本文是基于 2.8.0 版本的标准库。

在 VSCode 的 `agda-mode` 插件中，打开一个 Agda 文件，按 `Ctrl + C` 再按 `Ctrl + L` 来加载它。

或者直接在命令行中使用 `agda filename.agda` 编译它。

## 概览
### 布尔类型
布尔类型的定义如下
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

### 例子
现在来看一个大例子：自然数乘法交换律的证明。

其中少量使用了 `cong`，其定义将在之后等式理论节中说明。
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

## 列表
### 列表类型
列表类型的定义如下：
```agda
infixr 5 _∷_
data List {a} (A : Set a) : Set a where
  []  : List A
  _∷_ : (x : A) (xs : List A) → List A
```

这里用的 `∷` 是单个字符。

常见函数也以类似 Haskell 中的方式归纳定义。

使用 `postulate` 关键字可以不加证明地给出结论。
```agda
postulate
  ≤-trans : {x y z : Nat} →
    x ≤ y ≡ true → y ≤ z ≡ true → x ≤ z ≡ true
  ≤-suc : (x : Nat) → x ≤ suc x ≡ true
```

### `Maybe` 类型
```agda
data Maybe {a} (A : Set a) : Set a where
  just : A → Maybe A
  nothing : Maybe A
```

### `with` 与 `in`
`with` 关键字可以提供更丰富的类型匹配，例如：

```agda
length-filter : {a} {A : Set a} (p : A → B)(l : List A) →
  length (filter p l) ≤ length l ≡ true
length-filter p [] = refl
length-filter p (x ∷ l) with p x
length-filter p (x ∷ l) | true = ?
length-filter p (x ∷ l) | false = ?
```

`keep` 函数可以同时给出一个结果值和证明，定义为：
```agda
keep : {a} → {A : Set a} → (x : A) → Σ A (x ≡_)
keep x = x , refl
```

如下例中得到的是 `(true, p x ≡ true)` 或 `(false, p x ≡ false)`.
```agda
filter-idem : {a} {A : Set a} (p : A → Bool) (l : List A) →
  (filter p (filter p l)) ≡ (filter p l)
filter-idem p [] = refl
filter-idem p (x ∷ l) with keep (p x)
filter-idem p (x ∷ l) | true , p'
  rewrite p' | p' | filter-idem p l = refl
filter-idem p (x ∷ l) | false , p'
  rewrite p' = filter-idem p l
```

在现在的版本中可以改为使用 `in` 关键字：
```agda
filter-idem : {a} {A : Set a} (p : A → Bool) (l : List A) →
  (filter p (filter p l)) ≡ (filter p l)
filter-idem p [] = refl
filter-idem p (x ∷ xs) with p x in p'
filter-idem p (x ∷ xs) | true rewrite p' | filter-idem p xs = refl
filter-idem p (x ∷ xs) | false = filter-idem p xs
```

## Internal Verification
Internal Verification 的想法是，值附带着命题。

### 向量类型
向量类型的定义如下：
```agda
infixr 5 _∷_

data Vec (A : Set a) : ℕ → Set a where
  []  : Vec A zero
  _∷_ : ∀ (x : A) (xs : Vec A n) → Vec A (suc n)
```

这给出了一种定长数组。

### 关系
Relation 相关部分代码如下：
```agda
REL : Set a → Set b → (ℓ : Level) → Set (a ⊔ b ⊔ suc ℓ)
REL A B ℓ = A → B → Set ℓ

Rel : Set a → (ℓ : Level) → Set (a ⊔ suc ℓ)
Rel A ℓ = REL A A ℓ

Reflexive : Rel A ℓ → Set _
Reflexive _∼_ = ∀ {x} → x ∼ x

Transitive : Rel A ℓ → Set _
Transitive _∼_ = Trans _∼_ _∼_ _∼_
```

### Dependent Sum
Σ-类型是一种笛卡尔积的泛化，不同的是第二个元素的类型依赖于第一个元素。

其定义如下：
```agda
record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    fst : A
    snd : B fst

open Σ public

infixr 4 _,_
```

这可以给出一种定义正整数的方法（这里使用了 `λ` 生成匿名函数）：
```agda
ℕ⁺ : Set
ℕ⁺ = Σ ℕ (λ n → iszero n ≡ false)

suc⁺ : ℕ⁺ → ℕ⁺ 
suc⁺ (x , p) = (suc x , refl)

_+⁺_ : ℕ⁺ → ℕ⁺ → ℕ⁺
(x , p) +⁺ (y , q) = x + y , iszerosum2 x y p

_*⁺_ : ℕ⁺ → ℕ⁺ → ℕ⁺
(x , p) *⁺ (y , q) = (x * y , iszeromult x y p q)
```

## 等式理论
### 等式理论
有许多定理有助于我们进行 `rewrite`，一些常见内容如下：
```agda
sym : Symmetric {A = A} _≡_
sym refl = refl

trans : Transitive {A = A} _≡_
trans refl eq = eq

subst : Substitutive {A = A} _≡_ ℓ
subst P refl p = p

cong : ∀ (f : A → B) {x y} → x ≡ y → f x ≡ f y
cong f refl = refl

cong-app : ∀ {A : Set a} {B : A → Set b} {f g : (x : A) → B x} →
           f ≡ g → (x : A) → f x ≡ g x
cong-app refl x = refl
```

### 等式推理
等式推理相关设施的简化版定义如下：
```agda
infix 1 begin_
infixr 2 _≡⟨⟩_ _≡⟨_⟩_
infix 3 _∎

begin_ : ∀ {x y : A}
  → x ≡ y
  → x ≡ y
begin x≡y = x≡y

_≡⟨⟩_ : ∀ (x : A) {y : A}
  → x ≡ y
  → x ≡ y
x ≡⟨⟩ x≡y = x≡y

_≡⟨_⟩_ : ∀ (x : A) {y z : A}
  → x ≡ y
  → y ≡ z
  → x ≡ z
x ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

_∎ : ∀ (x : A)
  → x ≡ x
x ∎ = refl
```

一个用例如下：
```agda
+-suc : (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n = refl
+-suc (suc m) n =
  begin
    (suc m) + suc n
  ≡⟨ refl ⟩
    suc (m + suc n)
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m + n))
  ∎
```

有时 `cong` 的第一个参数很难用 `a +_` 之类的方式表达，就可使用 `λ` 生成匿名函数，如 `λ h → map (foldl _⊙_ o) ([] ∷ h)`.

## 拾遗
### Record
可以用 `record` 关键字把一组对象的某些性质聚合起来，在使用时可用 `.fieldname` 调用。

下面的例子相当于是说在自然数类型下 `+` 构成半群的二元运算，`(0, +)` 构成幺半群的幺元和二元运算。
```agda
open import Data.Nat using (_+_)

record IsSemigroup {A : Set} (_⊕_ : A → A → A) : Set where
  field assoc : ∀ x y z → (x ⊕ y) ⊕ z ≡ x ⊕ (y ⊕ z)

open import Data.Nat.Properties using (+-assoc)
ℕ-add-is-semigroup : IsSemigroup _+_
ℕ-add-is-semigroup .assoc = +-assoc

record IsMonoid {A : Set} (e : A) (_⊕_ : A → A → A) : Set where
  field
    is-semigroup : IsSemigroup _⊕_
    identityˡ : ∀ x → e ⊕ x ≡ x
    identityʳ : ∀ x → x ⊕ e ≡ x

open import Data.Nat.Properties using (+-identityˡ; +-identityʳ)
ℕ-add-is-monoid : IsMonoid 0 _+_
ℕ-add-is-monoid .is-semigroup = ℕ-add-is-semigroup
ℕ-add-is-monoid .identityˡ = +-identityˡ
ℕ-add-is-monoid .identityʳ = +-identityʳ
```

### 否定
有时我们希望证明某个东西的否定 `¬ x`，这定义为 `x → ⊥`，其中 `⊥` 代表 falsity 是一个空类型。

两个东西不等价 `expr1 ≢  expr2` 就是 `¬ (expr1 ≡ expr2)`.

它们的证法参考：
```agda
init-is-not-homomorphism : ∀ {_⊗_} (m : IsSemigroup _⊗_)
  → ¬ IsHomomorphism NList-++′-is-semigroup m (init {ℕ})
init-is-not-homomorphism {_⊗_} m H = ¬K K
  where
    ¬K : [ 0 ] ++ [ 1 ] ≢  [ 0 ] ++ [ 2 ]
    ¬K ()
    K : [ 0 ] ++ [ 1 ] ≡ [ 0 ] ++ [ 2 ]
    K =
      begin
        [ 0 ] ++ [ 1 ]
      ≡⟨ ? ⟩ -- 一定量的证明
        [ 0 ] ++ [ 2 ]
      ∎
```

我们使用 absurd pattern 定义出了函数 `¬K`，使得 `¬K K` 结果为 `⊥`.

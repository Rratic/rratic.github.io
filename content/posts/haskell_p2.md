+++
title = "Haskell 学习（二）"
description = "函数式的典型内容：幺半群、函子、应用、单子。"
date = 2025-08-27

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "计算机", "Haskell"]
+++

前置知识
- 一般的抽象代数基础
- [Haskell 学习（一）](/posts/haskell-p1/)
- 一般的范畴论基础（可选）

关于 Haskell 有一句广泛流传的话
> 一个单子说白了不过就是自函子范畴上的一个幺半群而已，这有什么难以理解的？

现在就来理清 Haskell 中的对应的概念。

## 幺半群
`Monoid` 是幺半群类型类。

幺半群在 Haskell 中表现为：
* 有二元运算 `<>`，满足结合律
* 有单位元，表达为 `mempty`

`[a]` 是幺半群。
```hs
ghci> [1,2,3] <> [1,2,3]
[1,2,3,1,2,3]
ghci> mappend [1,2,3] [4,5,6]
[1,2,3,4,5,6]
ghci> mappend [1,2,3] mempty
[1,2,3]
ghci> mconcat [[1,2,3], mempty, [4,5,6]]
[1,2,3,4,5,6]
```

## 函子
函子的类型类原型是：[^omit]
```hs
class Functor f where
  fmap :: (a -> b) -> f a
```

我们可以给之前的 `Option` 实现 `Functor`
```hs
data Option a = None | Some a deriving (Show)

instance Functor Option where
  fmap f (Some a) = Some $ f a
  fmap f (None)   = None
```

有：
```hs
ghci> fmap (+1) None
None
ghci> fmap (+1) $ Some 2
Some 3
ghci> (+1) <$> (Some 2)
Some 3
```

## 应用
应用的类型类原型是：
```hs
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

我们可以定义：
```hs
data Option a = None | Some a deriving (Show)

instance Functor Option where
  fmap f (Some a) = Some $ f a
  fmap f (None)   = None

instance Applicative Option where
  pure                  = Some
  (Some f) <*> (Some x) = Some $ f x
  (None)   <*> _        = None
  _        <*> (None)   = None
```

从而
```hs
ghci> import Data.Char
ghci> (Some isUpper) <*> (Some 'a')
Some False
```

一个神奇的例子是：
```hs
ghci> data Person = Person String Int Int deriving (Show)
ghci> Person <$> (Some "Alice") <*> (Some 1) <*> (Some 2)
Some (Person "Alice" 1 2)
ghci> :t Person
Person :: String -> Int -> Int -> Person
ghci> :t Person <$> (Some "Alice")
Person <$> (Some "Alice") :: Option (Int -> Int -> Person)
ghci> :t Person <$> (Some "Alice") <*> (Some 1)
Person <$> (Some "Alice") <*> (Some 1) :: Option (Int -> Person)
ghci> :t Person <$> (Some "Alice") <*> (Some 1) <*> (Some 2)
Person <$> (Some "Alice") <*> (Some 1) <*> (Some 2)
  :: Option Person
```

## 单子
单子的类型类原型是：
```hs
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

其中
* `>>=` 表义为 flatmap/bind
* `return = pure` 表义为 wrap

一个例子是
```hs
ghci> [1,2,3] >>= (\x -> [x,x])
[1,1,2,2,3,3]
```

让我们继续定义
```hs
instance Monad Option where
  (Some x) >>= f = f x
  (None)   >>= f = None

safeDiv :: Option Int -> Option Int -> Option Int
safeDiv xm ym = xm >>= (\x ->
                ym >>= (\y ->
                  if y == 0 then None
                            else return (x `div` y)))
```

`safeDiv` 的定义也可以使用 `do` 语法糖
```hs
safeDiv xm ym = do
           x <- xm
		   y <- ym
           if y == 0 then None
                     else return (x `div` y)
```

有
```hs
ghci> safeDiv (Some 4) $ safeDiv (Some 1) (Some 0)
None
```

`IO` 是典型的单子。

我们在文件中写
```hs
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!" >>= (\_ -> putStrLn "Hello, World!")
```

或将 `main` 写成
```hs
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Hello, World!"
```

就可得到
```hs
ghci> :load Main
[1 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
ghci> :main
Hello, Haskell!
Hello, World!
```

一个包含输入的例子：
```hs
module Main where

main :: IO ()
main = do
  putStrLn "Say something!"
  s <- getLine
  putStrLn ("Echo! " ++ s)
```

[^omit]: 在 GHCi 中使用命令 `:info Functor` 可以看到更详细的信息。一部分不必要的内容被省略了，下同。

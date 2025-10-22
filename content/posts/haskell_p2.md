+++
title = "Haskell 学习（二）：单子"
description = "函数式的典型内容：幺半群、函子、应用、单子。"
date = 2025-08-27
updated = 2025-10-22

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "计算机", "函数式编程", "Haskell"]
+++

前置知识
- 一般的抽象代数基础
- [Haskell 学习（一）](/posts/haskell-p1/)
- 一般的范畴论基础（可选）

关于 Haskell 有一句广泛流传的话
> 一个单子说白了不过就是自函子范畴上的一个幺半群而已，这有什么难以理解的？

现在就来理清 Haskell 中的对应的概念。

## IO
我们先从 `IO` 开始。

在 Haskell 中，函数是没有副作用的（正如数学中的函数不会进行输出一样），而 `IO` 类允许*引入*副作用。

我们可以把 `IO` 类型的定义视作：
```hs
type IO a = World -> (a, World)
```

然后可以使用相关函数进行 IO 操作：
```hs
getChar :: IO Char
putChar :: Char -> IO ()
return :: a -> IO a
```

使用 `do` 语法糖，我们可以顺序地执行
```hs
myGetLine :: IO String
myGetLine = do
  x <- getChar
  if x == '\n'
    then
      return []
    else do
      xs <- myGetLine
      return (x : xs)
```

就有：
```hs
ghci> str <- myGetLine
It's a red fox.
ghci> str
"It's a red fox."
```

我们在文件中写
```hs
module Main where

main :: IO ()
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

## 幺半群
`Monoid` 是幺半群类型类。

幺半群在数学上具有
* 满足结合律的二元运算，在 Haskell 中表现为 `<>`
* 单位元，在 Haskell 中表现为 `mempty`

例如，列表 `[a]` 是幺半群。
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
Haskell 中函子这个类型类原型是：[^omit]
```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

我们*希望*它对应到数学上协/共变函子的定义，这给出的额外要求是：
* `fmap id === id`
* `fmap (f . g) === fmap f . fmap g`

让我们测试这一点：
```hs
ghci> fmap show $ return True
"True"
```

这是因为 `show` 的类型是 `Show a => a -> String`，从而 `fmap` 把一个 `IO Bool` 映到了 `IO String`.

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

## 应用函子
我们希望定义一些一般版本的 `fmap`，如：
```hs
fmap0 :: a -> f a
fmap1 :: (a -> b) -> f a -> f b
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
...
```

它们可以由如下方式组合：
```hs
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

fmap0 = pure
fmap1 g x = pure g <*> x = fmap g x = g <$> x
fmap2 g x y = g <$> x <*> y
fmap3 g x y z = g <$> x <*> y <*> z
```

我们发现 `Applicative` 构成一个自函子范畴（具体来说，由类型范畴到类型范畴的函子构成，而且它是一个幺半范畴）上的幺半群（本质上是自然变换的横合成）。

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

  (>>) :: m a -> m b -> m b
    m >> k = m >>= \_ -> k

  return :: a -> m a
  return = pure
```

其中 `>>=` 表义为 `flatmap/bind`，`ma >>= mb` 等价于：
```hs
do a <- ma
  mb a
```

因此我们可以写
```hs
ghci> [1,2,3] >>= (\x -> [x,0])
[1,0,2,0,3,0]
ghci> putStrLn "Hello, Haskell!" >>= (\_ -> putStrLn "Hello, World!")
Hello, Haskell!
Hello, World!
```

我们发现 `Monad` 确实构成一个自函子范畴（还是由类型范畴到类型范畴的函子构成）上的幺半群（本质上是自然变换的纵合成）。

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

[^omit]: 在 GHCi 中使用命令 `:info Functor` 可以看到更详细的信息。一部分不必要的内容被省略了，下同。

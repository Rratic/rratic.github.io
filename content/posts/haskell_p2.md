+++
title = "Haskell 学习（二）：单子与副作用"
description = "函数式语言如何引入副作用：函子、应用函子、单子。"
date = 2025-08-27
updated = 2025-10-26

[extra]
toc = true
math = true

[extra.cover]
image = "/images/cover/haskell_p2.png"

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "计算机", "函数式编程", "Haskell"]
+++

封面图为 Monad 类型类的声明，使用字体 JetBrains Mono.

---

前置知识
- 一般的抽象代数基础
- [Haskell 学习（一）](/posts/haskell-p1/)
- 一般的范畴论基础

关于 Haskell 有一句广泛流传的话
> 一个单子说白了不过就是自函子范畴上的一个幺半群而已，这有什么难以理解的？

这句话有一点不负责任。现在我们来尝试理清 Haskell 中对应的概念。

## 副作用
在 Haskell 中，函数是没有副作用的，正如数学中的函数不会进行输出一样。

而 `IO` 类则以一种方式间接的方式*引入*了副作用。

我们可以把 `IO` 类型的定义视作：
```hs
type IO a = World -> (a, World)
```

然后可以使用相关函数进行 IO 操作：
```hs
getChar :: IO Char
putChar :: Char -> IO ()
putStrLn :: String -> IO ()
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

并用 `:load Main` 加载模块就可得到效果：
```hs
ghci> :main
Hello, Haskell!
Hello, World!
```

## 抽象层次
半群与幺半群是计算机中很容易出现的事物。

如果以一串函数复合为元素，考虑复合，或以一段程序为元素，定义复合为把两段程序紧挨着拼起来，那么此种意义下它们就是半群。

如果进一步允许恒等函数/空程序段，就能构成幺半群。

这将意味着，如果想要良好地处理副作用，就需要研究幺半群的结构和如何保持幺半群。

### 半群与幺半群
`Semigroup` 半群类型类的原型如下：[^omit]
```hs
class Semigroup a where
  (<>) :: a -> a -> a
```

我们要求 `<>` 是一个满足结合律的二元运算（尽管编译器无法强制检查这一点）。

易知列表类型 `[a]` 是半群。
```hs
ghci> [1..3] <> [4..6]
[1,2,3,4,5,6]
```

`Monoid` 幺半群类型类的原型如下：
```hs
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

其中 `mempty` 是需要定义的，对应单位元；而 `mappend` 与 `mconcat` 可由二元运算和单位元得到。

列表 `[a]` 同样是幺半群。
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

`Ordering` 类型也是幺半群，其原型为 `data Ordering = LT | EQ | GT`，可用于多个分量的数据的大小比较。
```hs
ghci> LT <> GT <> EQ
LT
ghci> EQ <> GT
GT
```

### Hask 范畴
我们所说 $\mathbf{Hask}$ 范畴是指：
* 以类型（如 `Int` 等）为对象
* 以函数（如 `isUpper` 等）为态射
* 单位态射为 `id`
* 态射复合是 `.`

不过，这个说法是不严格的，例如说：
* Haskell 的惰性求值特性会导致等式与求值策略有关
* `seq` 与性能分析等可以区分细节
* 存在 `undefined` 等特殊特性

如果我们需要讨论 `Functor` 等高级特性，还需要注意两个 Haskell 语言特性带来的影响。

Functor 和 Monad 是 $\mathbf{Hask}$-enriched 的（参考 [enriched category theory in nLab](https://ncatlab.org/nlab/show/enriched+category+theory) 页面），这意味着它们所定义的那些操作本身就是 Haskell 中的态射，只不过是高阶函数。

Functor 和 Monad 是自带 strength 的。

### 函子
我们来考虑前述 $\mathbf{Hask}$ 范畴上的自函子。

`Functor` 函子类型类的原型是：
```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap . const
```

例如说，`IO` 实现了 `Functor`，其中 `IO` 本身将对象（如类型 `a`）映到对象（类型 `IO a`），函数 `fmap` 则将 `a -> b` 的态射映到 `IO a -> IO b` 的态射。

我们希望它对应到数学上协/共变函子的定义，这给出的额外要求是：
* `fmap id` 恒等于 `id`
* `fmap (f . g)` 恒等于 `fmap f . fmap g`

让我们测试这一点：
```hs
ghci> fmap show $ return True
"True"
```

这是因为 `show` 的类型是 `Show a => a -> String`，从而 `fmap` 把一个 `IO Bool` 映到了 `IO String`.

我们可以给之前的 `Option` 实现 `Functor`.
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

### 应用函子
我们来考虑 $\mathbf{Hask}$ 范畴到自身的 lax monoidal functor（参考 [monoidal functor in nLab](https://ncatlab.org/nlab/show/monoidal+functor) 页面，是通过特定的自然变换保持幺半结构（张量积和单位元）的函子）。

`Applicative` 类型类的原型如下：
```hs
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
```

其中必须实现的是 `pure`，另可选择实现 `<*>` 与 `liftA2` 之一。

它们可以由如下方式组合出：
```hs
fmap0 :: a -> f a
fmap0 = pure

fmap1 :: (a -> b) -> f a -> f b
fmap1 g x = pure g <*> x = fmap g x = g <$> x

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = g <$> x <*> y

fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 g x y z = g <$> x <*> y <*> z
```

我们可以接着定义：
```hs
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

### 单子
现在我们可以考虑把副作用包装起来了。

`Monad` 单子类型类的原型是：
```hs
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
    m >> k = m >>= \_ -> k

  return :: a -> m a
  return = pure -- 不要更改此定义
```

其中必须实现的是 `>>=`，它表义为 `flatmap/bind`.

`ma >>= mb` 等价于：
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

我们的要求是:
* `return x >>= f` 等价于 `f x`
* `mx >>= return` 等价于 `mx`
* `(mx >>= f) >>= g` 等价于 `mx >>= (\x -> f x >>= g)`

我们发现它确实构成一个 $\mathbf{Hask}$ 范畴上的自函子范畴上的幺半群。

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

## 为了副作用
让我们从抽象的定义中抽身，看看现在究竟能做到怎样的副作用处理。

`Monad` 的定义使得我们可以进行顺序计算，并在计算时携带类型安全的上下文/副作用。

### 基本副作用
`Maybe` 类型的原型是：
```hs
data Maybe a = Nothing | Just a
```

这将允许计算失败。
```hs
safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

result :: Maybe Int
result = do
  x <- Just 6
  y <- safeDiv x 2
  z <- safeDiv y 0
  return z
```

---

`Either` 类型的原型是：
```hs
data Either a b = Left a | Right b
```

它可以这样用：`Left` 与 `Right` 一边存储异常提示，一边存储返回值。

---

列表作为单子，副作用语义是一个计算可能产生多个结果，我们会对所有可能性的笛卡尔积遍历。

### 上下文副作用
下文中提到的 `Monad` 都需通过以下方式导入

```hs
import Control.Monad.Reader
```

GHCi 可能会提示说它在一个隐藏的 package "mtl-2.3.1" 里，需要运行指令 `:set -package mtl` 来导入。

`Reader` 类型的原型是 `type Reader r = ReaderT r Identity`，为方便起见，把它视作 `Reader r a`，其含义是：`r` 是只读的环境，`a` 是你操作的值。[^reader]

一个例子如下：
```hs
import Control.Monad.Reader

tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"
```

这将给出：
```hs
ghci> putStrLn $ runJerryRun
Who is this? This is Tom.
Who is this? This is Jerry.
```

这可用于依赖注入、配置传递。

---

`Writer` 类型同理，可视作 `Monoid w => Writer w a`。每次计算会产生一个辅助的输出，其将被 `mappend` 到 `w` 中。

这可用于日志、计数器等。

```hs
import Control.Monad.Writer

compute :: Writer [String] Int
compute = do
  tell ["Starts here ###"]
  let x = 10
  tell ["Initial: " ++ show x]
  let y = x * 2
  tell ["Multiply by 2: " ++ show y]
  
  let z = y + 5
  tell ["Add by 5: " ++ show z]
  
  tell ["Done."]
  return z

example :: IO ()
example = do
  let (result, log) = runWriter compute
  putStrLn $ "Result: " ++ show result
  putStrLn "Log:"
  mapM_ putStrLn log
```

---

`State` 同样可看作 `State s a`，可看作 `Reader` 与 `Writer` 的结合。

```hs
import Control.Monad.State

type Account = Int

deposit :: Int -> State Account ()
deposit amount = modify (+ amount)

withdraw :: Int -> State Account Bool
withdraw amount = do
  balance <- get
  if amount <= balance
    then modify (subtract amount) >> return True
    else return False

example :: (Bool, Account)
example = runState (do
  put 10
  deposit 100
  deposit 50
  success <- withdraw 200
  return success) 1000
```

### 工具 Functor
`Data.Functor` 提供了一些工具性质的函子。

```hs
ghci> import Data.Functor.Identity
ghci> fmap (+1) (Identity 0)
Identity 1
ghci> :{
ghci| do
ghci|   x <- Identity 10
ghci|   y <- Identity (x + 5)
ghci|   pure (x + y)
ghci| :}
Identity 25
ghci> import Data.Functor.Const
ghci> fmap (++ "World") (Const "Hello")
Const "Hello"
ghci> Const [1, 2, 3] <*> Const [4, 5, 6]
Const [1,2,3,4,5,6]
```

### 高级 Monad
Haskell 中还有更加强大的控制流。

以下在 GHCi 中需要设置 `:set -package transformers`.

`Cont` 可以做 continuation-passing style 计算，可以实现任意跳转、提前返回、复杂错误处理。

`Select` 是 `Cont` 的一个特化版本，可解决搜索和优化问题。

`Free` 可以将任意 `Functor` 提升为 `Monad`.

关于它们的详细内容或许会在之后的文章中讨论。

---

事实上，在实际的 Haskell 开发中很少直接使用 `State s` 或 `Writer w` 等纯的单子，而是组合 Monad 变换器，形如：

```hs
type MyApp = ReaderT Config (StateT AppState (ExceptT Error IO))
```

[^omit]: 在 GHCi 中使用命令 `:info Semigroup`（可简写为 `:i Semigroup`）可以看到更详细的信息。一部分不必要的内容被省略了，下同。
[^reader]: 参考了 [A Simple Reader Monad Example](https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html) 的看法，下面的示例也来自此文。

+++
title = "密码学（一）：异或密码与 AES"
description = "使用 Haskell 完成的 The Cryptopals Crypto Challenges - Set 1"
date = 2025-09-22
updated = 2025-10-30

[extra]
toc = true
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "密码学"]
+++

{{ ref_index(to = "cryptography") }}

注：曾经我使用别的语言（Julia）写过类似的内容，但没有良好的解耦。因此改用 Haskell 进行更清晰的实现，读者也可使用自己熟悉的语言实现。

本文实现 [The Cryptopals Crypto Challenges](https://cryptopals.com/) 的基础练习集 Set 1.

## 格式转换
> Always operate on raw bytes, never on encoded strings. Only use hex and base64 for pretty-printing.

```hs
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe

type Raw = [Bool]

readSized :: Int -> Raw -> Int
readSized sz raw = sum [if raw !! i then 2 ^ (sz - i - 1) else 0 | i <- [0 .. sz - 1]]

writeSized :: Int -> Int -> Raw
writeSized sz int
  | sz == 0 = []
  | otherwise = writeSized (sz - 1) (int `div` 2) ++ [int `mod` 2 == 1]
```

### 解耦
一些常用的功能。

以指定的方式填充：
```hs
pad :: Int -> (Int -> a) -> [a] -> [a]
pad len gen list
  | pos == 0 = list
  | otherwise = list ++ map gen [(pos + 1) .. len]
  where
    pos :: Int = length list `mod` len
```

分段：
```hs
splits :: Int -> [a] -> [[a]]
splits len = unfoldr split
  where
    split [] = Nothing
    split xs = Just (splitAt len xs)
```

### Hex
这里实际上存在大小端的约定问题。按照 Challenge 1 的结果，对 $4=(0100)_2$，按从左至右顺序排布。

```hs
readHex :: String -> Raw
readHex = concatMap read1
  where
    read1 c = writeSized 4 $ fromJust $ lookup c (zip table [0 .. 15])
      where
        table = "0123456789abcdef"

writeHex :: Raw -> String
writeHex raw = map write4 (splits 4 padded)
  where
    padded = pad 4 (const False) raw
    write4 seq = table !! readSized 4 seq
      where
        table = "0123456789abcdef"
```

我们已经可以实现 Challenge 2

### Base64
```hs
writeBase64 :: Raw -> String
writeBase64 raw = map write6 (splits 6 padded)
  where
    padded = pad 6 (const False) raw
    write6 seq = table !! readSized 6 seq
      where
        table = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/"
```

有了这些东西我们已经可以实现 Challenge 1

### Ascii
```hs
readAscii :: String -> Raw
readAscii = concatMap (writeSized 8 . ord)

writeAscii :: Raw -> String
writeAscii raw = map write8 (splits 8 padded)
  where
    padded = pad 8 (const False) raw
    write8 seq = chr $ readSized 8 seq
```

## 异或密码
### 单位异或
Challenge 3 要求 do this by hand

观察十六进制编码，分为两个一段，即
```txt
1b 37 37 33 31 36 3f 78 15 1b 7f 2b 78 34 31 33 3d 78 39 78 28 37 2d 36 3c 78 37 3e 78 3a 39 3b 37 36
```

由于 78 大量出现，猜测它是空格，再猜测 39 是 a 发现是一致的。

英语词频分析的方法似乎有很多，包括[单/双/三/四字母组词频](http://practicalcryptography.com/cryptanalysis/text-characterisation/quadgrams/)、常用单词词频、首/末字母词频、双重字母词频。这里我们先使用单字母词频。

{% admonition(type="warning", title="词频失真") %}
随着 AI 生成内容的大量出现，英文词频发生了较大的失真。
{% end %}

这里的思路是直接计算赋值的和。

```hs
tableSpaceFrequency = 0.1918182

tableAsciiFrequency =
  [ 0.0651738,
    0.0124248,
    0.0217339,
    0.0349835,
    0.1041442,
    0.0197881,
    0.0158610,
    0.0492888,
    0.0558094,
    0.0009033,
    0.0050529,
    0.0331490,
    0.0202124,
    0.0564513,
    0.0596302,
    0.0137645,
    0.0008606,
    0.0497563,
    0.0515760,
    0.0729357,
    0.0225134,
    0.0082903,
    0.0171272,
    0.0013692,
    0.0145984,
    0.0007836
  ]

evalPhrase :: String -> Float
evalPhrase str = sum $ map eval1 str
  where
    eval1 c
      | c == ' ' = tableSpaceFrequency
      | isAsciiLower c = tableAsciiFrequency !! (ord c - ord 'a')
      | isAsciiUpper c = tableAsciiFrequency !! (ord c - ord 'A')
      | otherwise = 0.0
```

另一种计算思路是比较频率的分布与英文词频的相似程度。

```hs
evalPhrase :: String -> Float
evalPhrase str = sum $ map (\i -> (freqs !! i - tableAsciiFrequency !! i) ^ 2 / (tableAsciiFrequency !! i)) [0 .. 25]
  where
    freqs :: [Float]
    freqs = [num i / fromIntegral (length str) | i <- [0 .. 25]]
      where
        num i = foldr (\chr cnt -> if ord chr - ord 'a' == i || ord chr - ord 'A' == i then cnt + 1 else cnt) 0 str
```

不过它在 Haskell 中就太慢了，以下还是使用前者。

现在来实现 Challenge 4

检测的思路是：看所有解密可能中评分最高的。

```hs
performScXor :: Int -> String -> String
performScXor n = map perform
  where
    perform c = chr $ xor (ord c) n

detectScXor :: String -> (Float, String)
detectScXor str = maximum (map resultDecrypt [1 .. 127])
  where
    resultDecrypt x = (\y -> (evalPhrase y, y)) $ performScXor x str

challenge_4 :: [String] -> (String, String)
challenge_4 list = (snd $ fst best, snd best)
  where
    best = maximum $ map (\str -> (detectScXor str, str)) strings
    strings = map (writeAscii . readHex . pad 60 (const '0')) list
```

得到的结果为：

```hs
("Now that the party is jumping\n","{ZB\NAKA]TA\NAKA]P\NAKETGAL\NAK\\F\NAK_@XE\\[R?")
```

### 多位异或
Challenge 5 也是易做的：

```hs
performRpXor :: String -> String -> String
performRpXor key str = map (\(id, c) -> chr $ xor (ord c) (ord $ key !! mod id (length key))) indexed
  where
    indexed = zip [0 .. length str - 1] str
```

Challenge 6 比较有挑战，文本中指出的工作流程如下：

1. 令 KEYSIZE 为猜测的密钥长，不妨取 2 ~ 40
2. 写一个计算 edit distance/Hamming distance 的函数
3. 对每个 KEYSIZE，从文本中依次取出长度为它的两段，计算 edit distance 并除以 KEYSIZE
4. 上述的约化结果中最小的可能是正确的 KEYSIZE（为保证正确性，可能保留多个小的、计算多个片段）
5. 现在把原文本切成 KEYSIZE 大小的块
6. 对这些块作转置，即，考虑每个块的第 k 位，组成新块
7. 每个块的做法即单位异或解密的方法（如果没有解密是没有考虑字符之间的关系）
8. 现在把它们拼在一起

```hs
evalEditDistance :: (Eq a) => [a] -> [a] -> Int
evalEditDistance x y = foldr (\i c -> if x !! i == y !! i then c else c + 1) 0 [0 .. length x - 1]

-- 其余内容略去
```

## AES
### 有密钥解密
Challenge 7 部分略过。

### 检测
Challenge 8 中，用到 ECB 是无状态且确定的，相同的 16 字节段的加密结果一致，我们去检测是否存在这样的情况。

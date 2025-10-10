+++
title = "【草稿】密码学（一）：异或密码与 AES"
description = "使用 Haskell 完成的 The Cryptopals Crypto Challenges - Set 1"
date = 2025-09-22
updated = 2025-10-07

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "密码学", "Haskell"]
+++

注：曾经我使用别的语言（Julia）写过类似的内容，但没有良好的解耦。因此改用 Haskell 进行更清晰的实现，读者也可使用自己熟悉的语言实现。

关于密码学一个很好的书是 *Real World Cryptography*，兼具细节和高观点。但本文不会涉及太复杂的内容。

OverTheWire 提供的 [Krypton](https://overthewire.org/wargames/krypton/) 也是一个在线练习平台，但是内容过少。

本文实现 [The Cryptopals Crypto Challenges](https://cryptopals.com/) 的基础练习集 Set 1

## 技巧
TODO: 长行输入、获取剪贴板内容、获取指定 URL 内容

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
pad :: Int -> [a] -> (Int -> a) -> [a]
pad len list gen
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
    padded = pad 4 raw (const False)
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
    padded = pad 6 raw (const False)
    write6 seq = table !! readSized 6 seq
      where
        table = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "+/"
```

有了这些东西我们已经可以实现 Challenge 1

### Ascii
```hs
writeAscii :: Raw -> String
writeAscii raw = map write8 (splits 8 padded)
  where
    padded = pad 8 raw (const False)
    write8 seq = chr $ readSized 8 seq
```

## 文字密码
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

```hs
evalPhrase :: String -> Float
evalPhrase str = sum $ map eval1 str
  where
    eval1 c
      | c == ' ' = 0.1918182
      | isAsciiLower c = table !! (ord c - ord 'a')
      | isAsciiUpper c = table !! (ord c - ord 'A')
      | otherwise = 0.0
    table =
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
```

## AES
{{ todo() }}

+++
title = "Hash 与基于哈希的 table 实现"
description = "阅读 Rust 标准库的 Hash 与 RawTable 相关代码。"
date = 2025-12-08

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "算法"]
+++

本文参考的是 `1.90.0-nightly` 版本。

参考阅读
- Steven S. Skiena 的 *The Algorithm Design Manual* 第三版

## Hash
哈希的想法是：把任何对象编码到一个有限位的数字。

Rust 中的 `Hash` trait 的实例能够被 `Hasher` 实例作用得到哈希值。对使用 `#[derive(Hash)]` 的结构将依次对每个字段作 `hash()` 并结合。有两个要求：
- 如果还实现了 `Eq`，则必须满足 `k1 == k2 -> hash(k1) == hash(k2)`
- 前缀不重复，例如说 `("ab", "c")` 和 `("a", "bc")` 的哈希值不同

这个 trait 的声明大致如下：
```rs
pub trait Hash: marker::PointeeSized {
	#[stable(feature = "rust1", since = "1.0.0")]
    fn hash<H: Hasher>(&self, state: &mut H);

	#[stable(feature = "hash_slice", since = "1.3.0")]
    fn hash_slice<H: Hasher>(data: &[Self], state: &mut H)
    where
        Self: Sized,
    {
        for piece in data {
            piece.hash(state)
        }
    }
}
```

`Hasher` trait 的声明大致如下：
```rs
pub trait Hasher {
	#[stable(feature = "rust1", since = "1.0.0")]
    #[must_use]
    fn finish(&self) -> u64;

	/// 如果需要把长度作为前缀，额外调用 [`Hasher::write_length_prefix`]
	#[stable(feature = "rust1", since = "1.0.0")]
	fn write(&mut self, bytes: &[u8]);

    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_u8(&mut self, i: u8) {
        self.write(&[i])
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_u16(&mut self, i: u16) {
        self.write(&i.to_ne_bytes()) // 注：使用原生大小端约定
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_u32(&mut self, i: u32) {
        self.write(&i.to_ne_bytes())
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_u64(&mut self, i: u64) {
        self.write(&i.to_ne_bytes())
    }
    #[inline]
    #[stable(feature = "i128", since = "1.26.0")]
    fn write_u128(&mut self, i: u128) {
        self.write(&i.to_ne_bytes())
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_usize(&mut self, i: usize) {
        self.write(&i.to_ne_bytes())
    }

	#[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_i8(&mut self, i: i8) {
        self.write_u8(i as u8)
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_i16(&mut self, i: i16) {
        self.write_u16(i as u16)
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_i32(&mut self, i: i32) {
        self.write_u32(i as u32)
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_i64(&mut self, i: i64) {
        self.write_u64(i as u64)
    }
    #[inline]
    #[stable(feature = "i128", since = "1.26.0")]
    fn write_i128(&mut self, i: i128) {
        self.write_u128(i as u128)
    }
    #[inline]
    #[stable(feature = "hasher_write", since = "1.3.0")]
    fn write_isize(&mut self, i: isize) {
        self.write_usize(i as usize)
    }

	/// 为了 prefix-free
	/// 如果不在意 Hash-DoS 攻击而想提升性能也可跳过
	#[inline]
    #[unstable(feature = "hasher_prefixfree_extras", issue = "96762")]
    fn write_length_prefix(&mut self, len: usize) {
        self.write_usize(len);
    }

	/// 加上长度作为前缀是一种总是正确的实现方法
    ///
    /// ```
    /// fn write_str(&mut self, s: &str) {
    ///     self.write_length_prefix(s.len());
    ///     self.write(s.as_bytes());
    /// }
    /// ```
	///
	/// 如果你的 `Hasher` 是以 `usize` 为单位的话它是高效的
	/// 如果 `Hasher` 是以字节为单位的，则可使用 `b'\xFF'`
	/// 因为在正确的 UTF-8 编码中不会出现
	#[inline]
    #[unstable(feature = "hasher_prefixfree_extras", issue = "96762")]
    fn write_str(&mut self, s: &str) {
        self.write(s.as_bytes());
        self.write_u8(0xff);
    }
}
```

大部分类型的 `Hash` 实现不外乎是反复调用 `hash()` 或 `write_` 对应位数，间隔着 `write_length_prefix()`，如：
```rs
#[stable(feature = "rust1", since = "1.0.0")]
impl<K: Hash, V: Hash, A: Allocator + Clone> Hash for BTreeMap<K, V, A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_length_prefix(self.len());
        for elt in self {
            elt.hash(state);
        }
    }
}
```

Rust 默认使用的 `DefaultHasher` 包裹的是 [SipHash 1-3](https://131002.net/siphash) 算法，它兼顾了高效性和安全性。

## Hashing
我们先介绍一般的原理。在 Rust 中 `hash()` 的结果是一个 `u64` 值，但一般我们的 hash table 的容量远小于 `u64` 可表示的最大值，并且即使容量足够大也存在不同对象的哈希值相同（称为“哈希碰撞”）的可能。

为了处理哈希碰撞，一种方式拉链法 chaining 是在每个编号对应位置放一个链表。C++ 的 `std::unordered_set` 就是这么做的，并且对遍历操作进行了优化：给链表尾额外链接到了下一个非空链表。

另一种方法是开放寻址 open addressing，这会在目标槽被占据时寻找新的空槽（这最简单的方法是每次编号 +1 直到找到空槽）。这个算法在处理元素删除时会很麻烦：有两种简单的操作方式：（1）把后面的元素往前移（2）加标记，但都会导致元素变多时查找效率变差。

Robin Hood 哈希算法作了这样的优化：称距离指存储位置与哈希值直接对应的索引的差，在插入操作时，仍然每次 +1 地找，但若那个槽中元素的距离小于现在要插入的元素走过的距离，就插入到该槽中，改为寻找位置插入原本那个槽中的元素。

## RawTable
`HashSet` 及 `HashMap` 的声明如下：
```rs
pub struct HashSet<T, S = DefaultHashBuilder, A: Allocator = Global> {
    pub(crate) map: HashMap<T, (), S, A>,
}

pub struct HashMap<K, V, S = DefaultHashBuilder, A: Allocator = Global> {
    pub(crate) hash_builder: S,
    pub(crate) table: RawTable<(K, V), A>,
}
```

因此我们直接阅读 `RawTable` 的实现。

相关声明如下：
```rs
pub struct Bucket<T> {
    ptr: NonNull<T>,
}

pub struct RawTable<T, A: Allocator = Global> {
    table: RawTableInner,
    alloc: A,
    marker: PhantomData<T>, // 告知编译器，使行为类似于存储了 `T` 的实例
}

struct RawTableInner {
    // 用于从哈希值获取索引的掩码
    // 值就是 `bucket` 的数量 -1
    bucket_mask: usize,

    // 指向控制字节第一位
    ctrl: NonNull<u8>,

    // 在扩容前可以继续插入的元素个数（下界）
    growth_left: usize,

    // 含有的元素个数
    items: usize,
}
```

一个槽中的东西（一个 `bucket`）实际上包含了存储的数据、控制字节或元信息及额外的控制字节。

控制字节大致形如：
```rs
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub(crate) struct Tag(pub(super) u8);
impl Tag {
    pub(crate) const EMPTY: Tag = Tag(0b1111_1111);

    pub(crate) const DELETED: Tag = Tag(0b1000_0000);

    #[inline]
    pub(crate) const fn is_full(self) -> bool {
        self.0 & 0x80 == 0
    }

    #[inline]
    pub(crate) const fn is_special(self) -> bool {
        self.0 & 0x80 != 0
    }

    #[inline]
    pub(crate) const fn special_is_empty(self) -> bool {
        debug_assert!(self.is_special());
        self.0 & 0x01 != 0
    }

    #[inline]
    #[allow(clippy::cast_possible_truncation)]
    pub(crate) const fn full(hash: u64) -> Tag {
        const MIN_HASH_LEN: usize = if mem::size_of::<usize>() < mem::size_of::<u64>() {
            mem::size_of::<usize>()
        } else {
            mem::size_of::<u64>()
        };

        let top7 = hash >> (MIN_HASH_LEN * 8 - 7);
        Tag((top7 & 0x7f) as u8) // truncation
    }
}
```

`bucket_mask` 总是二的幂减一，从而可以直接使用按位与来获得索引。

计算容量时，使用如下方法。这意味着桶数量大于 8 时我们会保留 12.5% 的空槽。
```rs
#[inline]
fn bucket_mask_to_capacity(bucket_mask: usize) -> usize {
    if bucket_mask < 8 {
        bucket_mask
    } else {
        ((bucket_mask + 1) / 8) * 7
    }
}
```

探测插入的可用槽位涉及以下代码：
```rs
#[derive(Clone)]
struct ProbeSeq {
    pos: usize,
    stride: usize,
}

impl ProbeSeq {
    #[inline]
    fn move_next(&mut self, bucket_mask: usize) {
        debug_assert!(
            self.stride <= bucket_mask,
            "Went past end of probe sequence"
        );

        self.stride += Group::WIDTH; // 这个值是 8
        self.pos += self.stride;
        self.pos &= bucket_mask;
    }
}

impl RawTableInner {
	#[inline]
    unsafe fn find_insert_slot(&self, hash: u64) -> InsertSlot {
        let mut probe_seq = self.probe_seq(hash);
        loop {
            let group = unsafe { Group::load(self.ctrl(probe_seq.pos)) };

            let index = self.find_insert_slot_in_group(&group, &probe_seq);
            if likely(index.is_some()) {
                unsafe {
                    return self.fix_insert_slot(index.unwrap_unchecked());
                }
            }
            probe_seq.move_next(self.bucket_mask);
        }
    }
}
```

这里探测方式是第一次跳过一个组的长度，第二次跳过两个组的长度，依此类推。容易证明（槽数量为 2 的幂时）可以遍历所有的槽。

这里 `likely`（及对应的 `unlikely`）是用于分支预测优化的。

现在来看看 `insert` 的实现：
```rs
impl<T, A: Allocator> RawTable<T, A> {
	#[cfg_attr(feature = "inline-more", inline)]
    pub fn reserve(&mut self, additional: usize, hasher: impl Fn(&T) -> u64) {
        if unlikely(additional > self.table.growth_left) {
            unsafe {
                if self
                    .reserve_rehash(additional, hasher, Fallibility::Infallible)
                    .is_err()
                {
                    hint::unreachable_unchecked()
                }
            }
        }
    }

    #[inline]
    pub unsafe fn insert_in_slot(&mut self, hash: u64, slot: InsertSlot, value: T) -> Bucket<T> {
        let old_ctrl = *self.table.ctrl(slot.index);
        self.table.record_item_insert_at(slot.index, old_ctrl, hash);

        let bucket = self.bucket(slot.index);
        bucket.write(value);
        bucket
    }

	#[cfg_attr(feature = "inline-more", inline)]
    pub fn insert(&mut self, hash: u64, value: T, hasher: impl Fn(&T) -> u64) -> Bucket<T> {
        unsafe {
            let mut slot = self.table.find_insert_slot(hash);

            let old_ctrl = *self.table.ctrl(slot.index);
            if unlikely(self.table.growth_left == 0 && old_ctrl.special_is_empty()) {
                self.reserve(1, hasher);
                slot = self.table.find_insert_slot(hash);
            }

            self.insert_in_slot(hash, slot, value)
        }
    }
}
```

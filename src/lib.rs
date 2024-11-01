#![doc = include_str!("../README.md")]

use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    marker::PhantomData,
    ops::Deref,
};

pub use difficient_macros::Diffable;

#[cfg(feature = "chrono")]
mod chrono;
#[cfg(feature = "visitor")]
mod serde_visit;
#[cfg(feature = "uuid")]
mod uuid;

mod vec;

pub use vec::{DiffKey, VecChange, VecDiff};

#[cfg(feature = "visitor")]
pub use serde_visit::{AcceptVisitor, Enter, SerdeVariantTag, Visitor};

// *** Trait

/// The core trait of this library. Enables a value 'A' to be 'diffed' against
/// another value 'B' of the same type, returning a 'diff' representing the
/// difference between the two. The diff can then be applied to A to result in B.
///
/// Generally the user will implement this trait via the `derive(Diffable)` macro
/// rather manually.
pub trait Diffable<'a>: Sized {
    type Diff: Replace + Apply<Parent = Self>;

    fn diff(&self, other: &'a Self) -> Self::Diff;

    fn apply(&mut self, diff: &Self::Diff) -> Result<(), Vec<ApplyError>> {
        let mut errs = Vec::new();
        diff.apply_to_base(self, &mut errs);
        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, thiserror::Error)]
pub enum ApplyError {
    #[error("enum mismatch")]
    MismatchingEnum,
    #[error("missing key")]
    MissingKey,
    #[error("unexpected key")]
    UnexpectedKey,
}

/// A trait that must be implemented by any type that results from a 'diff' operation.
///
/// Generally this trait need not be implemented by the user. Instead look to use
/// generic wrapper types `AtomicDiff` and `DeepDiff`.
pub trait Replace {
    fn is_unchanged(&self) -> bool;
    fn is_replaced(&self) -> bool;
}

/// A trait that must implemented by any type that results from a 'diff' operation.
/// It allows the (child) 'diff' type to be 'applied' to the parent type, mutating
/// the parent in place to result in the value which is was originally diffed against.
///
/// Generally the user will not need to implement this type, as it will be done by
/// the `derive(Diffable)` macro
pub trait Apply {
    type Parent;
    fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>);
}

// *** Helper structs

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Id<T>(PhantomData<T>);

impl<T> Id<T> {
    pub fn new() -> Id<T> {
        Id(PhantomData)
    }
}

impl<T> Default for Id<T> {
    fn default() -> Self {
        Id(PhantomData)
    }
}

impl<T> Replace for Id<T> {
    fn is_unchanged(&self) -> bool {
        true
    }

    fn is_replaced(&self) -> bool {
        false
    }
}

impl<T> Apply for Id<T> {
    type Parent = T;
    fn apply_to_base(&self, _: &mut Self::Parent, _: &mut Vec<ApplyError>) {}
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
/// A generic type which can represent two possible 'diff' states.
/// Appropriate for primitive types which cannot be 'partially' changed (e.g. bool, i32)
/// c.f. DeepDiff which can also represent a partially-changed state
pub enum AtomicDiff<'a, T> {
    /// The diffed value is unchanged
    Unchanged,
    /// The diffed value is replaced
    Replaced(&'a T),
}

impl<'a, T: PartialEq> AtomicDiff<'a, T> {
    pub fn new(left: &T, right: &'a T) -> Self {
        if left == right {
            Self::Unchanged
        } else {
            Self::Replaced(right)
        }
    }
}

impl<'a, T> Replace for AtomicDiff<'a, T> {
    fn is_unchanged(&self) -> bool {
        matches!(self, AtomicDiff::Unchanged)
    }

    fn is_replaced(&self) -> bool {
        matches!(self, AtomicDiff::Replaced(_))
    }
}

impl<'a, T> Apply for AtomicDiff<'a, T>
where
    T: Clone,
{
    type Parent = T;
    fn apply_to_base(&self, source: &mut Self::Parent, _: &mut Vec<ApplyError>) {
        match self {
            AtomicDiff::Unchanged => {}
            AtomicDiff::Replaced(r) => *source = (*r).clone(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
/// A generic type which can represent three possible 'diff' states.
/// Appropriate for types which can be 'partially' changed (e.g. structs)
/// c.f. AtomicDiff which cannot represent a `Patched` state
pub enum DeepDiff<'a, Full, Patch> {
    /// The diffed value is unchanged
    Unchanged,
    /// The diffed value is partially changed - we will need to
    /// descend into the `Patched` value to find out exactly how
    Patched(Patch),
    /// The diffed value is replaced in full
    Replaced(&'a Full),
}

impl<'a, T, U> Replace for DeepDiff<'a, T, U> {
    fn is_unchanged(&self) -> bool {
        matches!(self, DeepDiff::Unchanged)
    }

    fn is_replaced(&self) -> bool {
        matches!(self, DeepDiff::Replaced(_))
    }
}

impl<'a, T, U> Apply for DeepDiff<'a, T, U>
where
    T: Diffable<'a> + Clone,
    U: Apply<Parent = T>, // <<T as Diffable>::Diff as Apply>::Parent = T,
{
    type Parent = T;

    fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
        match self {
            Self::Unchanged => {}
            Self::Patched(patch) => patch.apply_to_base(source, errs),
            Self::Replaced(r) => *source = (*r).clone(),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
/// A generic type which can represent two possible 'diff' states.
/// Appropriate for types which cannot be fully replaced - this only
/// really makes sense in the context of a derived 'diff' that has skipped fields
pub enum PatchOnlyDiff<Patch> {
    Unchanged,
    Patched(Patch),
}

impl<P> Replace for PatchOnlyDiff<P> {
    fn is_unchanged(&self) -> bool {
        matches!(self, PatchOnlyDiff::Unchanged)
    }

    fn is_replaced(&self) -> bool {
        false
    }
}

impl<P> Apply for PatchOnlyDiff<P>
where
    P: Apply,
{
    type Parent = <P as Apply>::Parent;

    fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
        match self {
            Self::Unchanged => {}
            Self::Patched(patch) => patch.apply_to_base(source, errs),
        };
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
/// A generic type which represents the possible change-states of a Key-Value type
/// (e.g. HashMap, BTreeMap)
pub enum KvDiff<'a, T, U> {
    Removed,
    Inserted(&'a T),
    Diff(U),
}

// ** Common impls ***

macro_rules! impl_diffable_for_primitives {
    ($($typ: ty)*) => ($(
        impl<'a> Diffable<'a> for $typ {
            type Diff = AtomicDiff<'a, Self>;

            fn diff(&self, other: &'a Self) -> Self::Diff {
                if self == other {
                    AtomicDiff::Unchanged
                } else {
                    AtomicDiff::Replaced(&other)
                }
            }
        }
    )*);
}

impl_diffable_for_primitives! {
    i8 i16 i32 i64 isize
    u8 u16 u32 u64 usize
    f32 f64
    bool
    &'static str
    String
}

macro_rules! kv_map_impl {
    ($typ: ident, $bounds: ident) => {
        impl<'a, K, V> Diffable<'a> for $typ<K, V>
        where
            K: $bounds + Eq + Clone + 'a,
            V: Diffable<'a> + Clone + 'a,
        {
            type Diff = DeepDiff<'a, Self, $typ<K, KvDiff<'a, V, V::Diff>>>;

            fn diff(&self, other: &'a Self) -> Self::Diff {
                let mut diffs: $typ<K, KvDiff<V, V::Diff>> = $typ::new();
                let mut all_unchanged = true;
                let mut all_replaced = true;
                for (k, v) in self.iter() {
                    let Some(other) = other.get(k) else {
                        all_replaced = false;
                        all_unchanged = false;
                        diffs.insert(k.clone(), KvDiff::Removed);
                        continue;
                    };
                    let diff = v.diff(other);
                    if diff.is_unchanged() {
                        // do 'nothing'
                        all_replaced = false;
                        continue;
                    } else {
                        all_replaced &= diff.is_replaced();
                        all_unchanged = false;
                        diffs.insert(k.clone(), KvDiff::Diff(diff));
                    }
                }
                for (k, v) in other.iter() {
                    if !self.contains_key(k) {
                        all_unchanged = false;
                        all_replaced = false;
                        diffs.insert(k.clone(), KvDiff::Inserted(v));
                    }
                }
                if all_unchanged {
                    DeepDiff::Unchanged
                } else if all_replaced {
                    DeepDiff::Replaced(other)
                } else {
                    DeepDiff::Patched(diffs)
                }
            }
        }

        impl<'a, K, V> Apply for $typ<K, KvDiff<'a, V, V::Diff>>
        where
            K: $bounds + Eq + Clone,
            V: Diffable<'a> + Clone,
        {
            type Parent = $typ<K, V>;

            fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
                for (k, v) in self.into_iter() {
                    match v {
                        KvDiff::Removed => match source.remove(&k) {
                            Some(_) => {}
                            None => errs.push(ApplyError::MissingKey),
                        },
                        KvDiff::Inserted(val) => {
                            match source.insert((*k).clone(), (*val).clone()) {
                                Some(_) => errs.push(ApplyError::UnexpectedKey),
                                None => {}
                            }
                        }
                        KvDiff::Diff(diff) => match source.get_mut(&k) {
                            Some(val) => diff.apply_to_base(val, errs),
                            None => errs.push(ApplyError::MissingKey),
                        },
                    }
                }
            }
        }
    };
}

kv_map_impl!(HashMap, Hash);
kv_map_impl!(BTreeMap, Ord);

impl<'a> Diffable<'a> for () {
    type Diff = Id<Self>;

    fn diff(&self, _: &Self) -> Self::Diff {
        Id::new()
    }
}

impl<'a, T> Diffable<'a> for Box<T>
where
    T: Diffable<'a>,
{
    type Diff = Box<T::Diff>;

    fn diff(&self, other: &'a Self) -> Self::Diff {
        Box::new(self.deref().diff(other.deref()))
    }
}

impl<T> Replace for Box<T>
where
    T: Replace,
{
    fn is_unchanged(&self) -> bool {
        self.deref().is_unchanged()
    }

    fn is_replaced(&self) -> bool {
        self.deref().is_replaced()
    }
}

impl<T> Apply for Box<T>
where
    T: Apply,
{
    type Parent = Box<T::Parent>;

    fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
        self.deref().apply_to_base(source, errs)
    }
}

impl<'a, T> Diffable<'a> for Option<T>
where
    T: Diffable<'a> + Clone + 'a,
{
    type Diff = DeepDiff<'a, Self, Option<T::Diff>>;

    fn diff(&self, other: &'a Self) -> Self::Diff {
        match (self, other) {
            (None, None) => DeepDiff::Unchanged,
            (None, Some(_)) | (Some(_), None) => DeepDiff::Replaced(other),
            (Some(l), Some(r)) => {
                let diff = l.diff(r);
                if diff.is_unchanged() {
                    DeepDiff::Unchanged
                } else if diff.is_replaced() {
                    DeepDiff::Replaced(other)
                } else {
                    DeepDiff::Patched(Some(diff))
                }
            }
        }
    }
}

impl<T> Apply for Option<T>
where
    T: Apply,
{
    type Parent = Option<T::Parent>;

    fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
        match (self, source) {
            (Some(diff), Some(src)) => diff.apply_to_base(src, errs),
            _ => errs.push(ApplyError::MismatchingEnum),
        }
    }
}

macro_rules! tuple_impl {
    ( $( $tup:ident $ix:tt ),* ) => {
        impl<'a, $( $tup ),*> Diffable<'a> for ( $( $tup, )* )
        where
            $( $tup: Diffable<'a> ),*
        {
            type Diff = ( $( $tup::Diff,)* );

            fn diff(&self, other: &'a Self) -> Self::Diff {
                (
                    $(
                        self.$ix.diff(&other.$ix),
                    )*
                )
            }
        }

        impl< $( $tup ),*> Replace for ($( $tup, )*)
        where
            $( $tup: Replace ),*
        {
            fn is_unchanged(&self) -> bool {
                    $(
                        self.$ix.is_unchanged() &&
                    )*
                    true
            }

            fn is_replaced(&self) -> bool {
                    $(
                        self.$ix.is_replaced() &&
                    )*
                    true
            }
        }

        impl< $( $tup ),*> Apply for ( $( $tup, )*)
        where
            $( $tup: Apply ),*
        {
            type Parent = ( $( $tup::Parent, )* );

            fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
                    $(
                        self.$ix.apply_to_base(&mut source.$ix, errs);
                    )*
            }
        }
    };
}

tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7, I 8);
tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5, G 6, H 7);
tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5, G 6);
tuple_impl!(A 0, B 1, C 2, D 3, E 4, F 5);
tuple_impl!(A 0, B 1, C 2, D 3, E 4);
tuple_impl!(A 0, B 1, C 2, D 3);
tuple_impl!(A 0, B 1, C 2);
tuple_impl!(A 0, B 1);
tuple_impl!(A 0);

#[cfg(test)]
pub mod tests {

    use super::*;

    // ** Test structs

    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize))]
    pub struct Parent {
        pub c1: Child1,
        pub c2: Vec<Child1>,
        pub c3: HashMap<i32, Child2>,
        pub val: String,
    }

    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize))]
    pub struct Child1 {
        pub x: i32,
        pub y: String,
    }

    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize))]
    pub struct Child2 {
        pub a: String,
        pub b: SomeChild,
        pub c: (),
    }

    #[derive(Debug, Clone, PartialEq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize))]
    pub enum SomeChild {
        C1(Child1),
        C2(Box<Child2>),
    }

    // *** Impls ***

    impl<'a> Diffable<'a> for Parent {
        type Diff = DeepDiff<'a, Self, ParentDiff<'a>>;

        fn diff(&self, other: &'a Self) -> Self::Diff {
            let c1 = self.c1.diff(&other.c1);
            let c2 = self.c2.diff(&other.c2);
            let c3 = self.c3.diff(&other.c3);
            let val = self.val.diff(&other.val);
            if c1.is_unchanged() && c2.is_unchanged() && c3.is_unchanged() && val.is_unchanged() {
                DeepDiff::Unchanged
            } else if c1.is_replaced() && c2.is_replaced() && c3.is_replaced() && val.is_replaced()
            {
                DeepDiff::Replaced(other)
            } else {
                DeepDiff::Patched(ParentDiff { c1, c2, c3, val })
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct ParentDiff<'a> {
        pub c1: <Child1 as Diffable<'a>>::Diff,
        pub c2: <Vec<Child1> as Diffable<'a>>::Diff,
        pub c3: <HashMap<i32, Child2> as Diffable<'a>>::Diff,
        pub val: <String as Diffable<'a>>::Diff,
    }

    impl<'a> Apply for ParentDiff<'a> {
        type Parent = Parent;

        fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
            self.c1.apply_to_base(&mut source.c1, errs);
            self.c2.apply_to_base(&mut source.c2, errs);
            self.c3.apply_to_base(&mut source.c3, errs);
            self.val.apply_to_base(&mut source.val, errs);
        }
    }

    impl<'a> Diffable<'a> for Child1 {
        type Diff = DeepDiff<'a, Self, Child1Diff<'a>>;

        fn diff(&self, other: &'a Self) -> Self::Diff {
            let x = self.x.diff(&other.x);
            let y = self.y.diff(&other.y);
            if x.is_unchanged() && y.is_unchanged() {
                DeepDiff::Unchanged
            } else if x.is_replaced() && y.is_replaced() {
                DeepDiff::Replaced(other)
            } else {
                DeepDiff::Patched(Child1Diff { x, y })
            }
        }
    }

    impl DiffKey for Child1 {
        type Key<'a> = i32;

        fn key(&self) -> Self::Key<'_> {
            self.x
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Child1Diff<'a> {
        pub x: <i32 as Diffable<'a>>::Diff,
        pub y: <String as Diffable<'a>>::Diff,
    }

    impl<'a> Apply for Child1Diff<'a> {
        type Parent = Child1;

        fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
            self.x.apply_to_base(&mut source.x, errs);
            self.y.apply_to_base(&mut source.y, errs);
        }
    }

    #[allow(clippy::unit_arg)]
    impl<'a> Diffable<'a> for Child2 {
        type Diff = DeepDiff<'a, Self, Child2Diff<'a>>;
        fn diff(&self, other: &'a Self) -> Self::Diff {
            let a = self.a.diff(&other.a);
            let b = self.b.diff(&other.b);
            #[allow(clippy::unit_arg)]
            let c = self.c.diff(&other.c);
            if a.is_unchanged() && b.is_unchanged() && c.is_unchanged() {
                DeepDiff::Unchanged
            } else if a.is_replaced() && b.is_replaced() && c.is_replaced() {
                DeepDiff::Replaced(other)
            } else {
                DeepDiff::Patched(Child2Diff { a, b, c })
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Child2Diff<'a> {
        pub a: <String as Diffable<'a>>::Diff,
        pub b: <SomeChild as Diffable<'a>>::Diff,
        pub c: <() as Diffable<'a>>::Diff,
    }

    impl<'a> Apply for Child2Diff<'a> {
        type Parent = Child2;

        fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
            self.a.apply_to_base(&mut source.a, errs);
            self.b.apply_to_base(&mut source.b, errs);
            self.c.apply_to_base(&mut source.c, errs);
        }
    }

    impl<'a> Diffable<'a> for SomeChild {
        type Diff = DeepDiff<'a, SomeChild, SomeChildDiff<'a>>;
        fn diff(&self, other: &'a Self) -> Self::Diff {
            match (self, other) {
                (Self::C1(left), Self::C1(right)) => {
                    let this = left.diff(right);
                    if this.is_unchanged() {
                        DeepDiff::Unchanged
                    } else if this.is_replaced() {
                        DeepDiff::Replaced(other)
                    } else {
                        DeepDiff::Patched(SomeChildDiff::C1(this))
                    }
                }
                (Self::C2(left), Self::C2(right)) => {
                    let this = left.diff(right);
                    if this.is_unchanged() {
                        DeepDiff::Unchanged
                    } else if this.is_replaced() {
                        DeepDiff::Replaced(other)
                    } else {
                        DeepDiff::Patched(SomeChildDiff::C2(this))
                    }
                }
                _ => DeepDiff::Replaced(other),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum SomeChildDiff<'a> {
        C1(<Child1 as Diffable<'a>>::Diff),
        C2(Box<<Child2 as Diffable<'a>>::Diff>),
    }

    impl<'a> Apply for SomeChildDiff<'a> {
        type Parent = SomeChild;

        fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
            match (self, source) {
                (SomeChildDiff::C1(diff), SomeChild::C1(src)) => diff.apply_to_base(src, errs),
                (SomeChildDiff::C2(diff), SomeChild::C2(src)) => diff.apply_to_base(src, errs),
                _ => errs.push(ApplyError::MismatchingEnum),
            }
        }
    }

    #[test]
    fn smoke_test() {
        fn dummy_child2() -> Child2 {
            Child2 {
                a: "ayeaye".into(),
                b: SomeChild::C1(Child1 {
                    x: 222,
                    y: "uuu".into(),
                }),
                c: (),
            }
        }

        let base = Parent {
            c1: Child1 {
                x: 123,
                y: "me".into(),
            },
            c2: vec![Child1 {
                x: 234,
                y: "yazoo".into(),
            }],
            c3: [(
                321,
                Child2 {
                    a: "ayeaye".into(),
                    b: SomeChild::C2(Box::new(dummy_child2())),
                    c: (),
                },
            )]
            .into_iter()
            .collect(),
            val: "hello".into(),
        };

        {
            let mut p1 = base.clone();
            let p2 = base.clone();
            let diff = p1.diff(&p2);
            assert!(matches!(diff, DeepDiff::Unchanged));
            assert!(p1.apply(&diff).is_ok());
            assert_eq!(p1, base);
        }

        let mello = "mello".to_string();

        {
            let mut p3 = base.clone();
            let mut p4 = p3.clone();
            p4.val = mello.clone();
            let diff = p3.diff(&p4);
            let expect = DeepDiff::Patched(ParentDiff {
                c1: DeepDiff::Unchanged,
                c2: VecDiff::Unchanged,
                c3: DeepDiff::Unchanged,
                val: AtomicDiff::Replaced(&mello),
            });
            assert_eq!(diff, expect);
            assert!(p3.apply(&diff).is_ok());
            assert_eq!(p3, p4);
        }

        {
            let mut p5 = base.clone();
            let dummy = dummy_child2();
            let bad_patch = DeepDiff::Patched(ParentDiff {
                c1: DeepDiff::Unchanged,
                c2: VecDiff::Unchanged,
                c3: DeepDiff::Patched(
                    [
                        (543, KvDiff::Removed),          // key does not exist
                        (321, KvDiff::Inserted(&dummy)), // key already exists
                    ]
                    .into_iter()
                    .collect(),
                ),
                val: AtomicDiff::Replaced(&mello),
            });
            let mut err = p5.apply(&bad_patch).unwrap_err();
            err.sort();
            assert_eq!(err, [ApplyError::MissingKey, ApplyError::UnexpectedKey]);
        }

        {
            let mut map1 = BTreeMap::new();
            let map2 = BTreeMap::from([("hello", "world")]);

            let diff = map1.diff(&map2);
            assert!(map1.apply(&diff).is_ok());
            assert_eq!(map1.get("hello"), Some(&"world"));
        }
    }
}

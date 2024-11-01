use std::cmp::Ord;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

use crate::PatchOnlyDiff;

use super::{AtomicDiff, DeepDiff, Id, KvDiff, Replace};

/// A trait that allows the user to extract information from a 'diff' type.
/// The visitor will visit the leaves of a diff breadth-first,
/// and be notified of changes as it does so. It can be used to collect the changes
/// together to, for example, export via HTTP.
pub trait AcceptVisitor {
    fn accept<V: Visitor>(&self, visitor: &mut V);
}

/// A trait to be that works in concert with `AcceptVisitor` to allow the user to extract
/// information from the diff.
///
/// Generally this trait need not be implemented by the user
pub trait Visitor {
    /// Descend into the internal structure of the diff
    fn enter(&mut self, val: Enter);
    /// Ascend one level of the internal structure of the diff
    fn exit(&mut self);
    /// The value at our current location was replaced
    fn replaced<T: serde::Serialize>(&mut self, val: T);
    /// The vector at our current location was spliced - i.e.
    /// `replace` elements were replaced with elements `values`, starting from `from_index`
    fn splice<T: serde::Serialize>(&mut self, from_index: usize, replace: usize, values: &[T]);
}

pub enum Enter {
    NamedField {
        name: &'static str,
        serde_rename: Option<&'static str>,
    },
    PositionalField(usize),
    Variant {
        name: &'static str,
        serde_rename: Option<&'static str>,
        serde_tag: SerdeVariantTag,
    },
    MapKey(String),
    Index(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SerdeVariantTag {
    External,
    Internal { tag: String },
    Adjacent { tag: String, content: String },
    Untagged,
}

impl<'a, T> AcceptVisitor for AtomicDiff<'a, T>
where
    T: serde::Serialize,
{
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            Self::Unchanged => {}
            Self::Replaced(v) => visitor.replaced(v),
        }
    }
}

impl<'a, T, U> AcceptVisitor for DeepDiff<'a, T, U>
where
    T: serde::Serialize,
    U: AcceptVisitor,
{
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            Self::Unchanged => {}
            Self::Patched(inner) => inner.accept(visitor),
            Self::Replaced(r) => visitor.replaced(r),
        }
    }
}

impl<P> AcceptVisitor for PatchOnlyDiff<P>
where
    P: AcceptVisitor,
{
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        match self {
            Self::Unchanged => {}
            Self::Patched(inner) => inner.accept(visitor),
        }
    }
}

impl<T> AcceptVisitor for Id<T> {
    fn accept<V: Visitor>(&self, _: &mut V) {
        // nothing to do
    }
}

impl<T> AcceptVisitor for Option<T>
where
    T: serde::Serialize + AcceptVisitor + Replace,
{
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        if let Self::Some(v) = self {
            // visitor.enter(Enter::Variant {
            //     name: "Some",
            //     serde_rename: None,
            //     serde_tag: SerdeVariantTag::Untagged,
            // });
            if !v.is_unchanged() {
                // NOTE: unlike elsewhere, we do NOT enter positional field here
                // Option<T> is a special case
                v.accept(visitor);
            }
            // visitor.exit();
        }
    }
}

impl<T> AcceptVisitor for Box<T>
where
    T: serde::Serialize + AcceptVisitor + Replace,
{
    fn accept<V: Visitor>(&self, visitor: &mut V) {
        if !self.is_unchanged() {
            self.accept(visitor);
        }
    }
}

macro_rules! tuple_impl {
    ( $( $tup:ident $ix:tt ),* ) => {
        impl<$( $tup ),*> AcceptVisitor for ( $( $tup, )* )
        where
            $( $tup: AcceptVisitor + Replace ),*
        {

            fn accept<V: Visitor>(&self, visitor: &mut V) {
                $(
                    if !self.$ix.is_unchanged() {
                        visitor.enter(Enter::PositionalField($ix));
                        self.$ix.accept(visitor);
                        visitor.exit();
                    }
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

macro_rules! kv_map_impl {
    ($typ: ident, $bounds: ident) => {
        impl<'a, K, V, U> AcceptVisitor for $typ<K, KvDiff<'a, V, U>>
        where
            K: $bounds + ToString + 'a,
            V: serde::Serialize + 'a,
            U: AcceptVisitor,
        {
            fn accept<Vis: Visitor>(&self, visitor: &mut Vis) {
                for (k, v) in self.iter() {
                    match v {
                        KvDiff::Removed => {
                            // not suported
                            unimplemented!()
                        }
                        KvDiff::Inserted(v) => {
                            visitor.enter(Enter::MapKey(k.to_string()));
                            visitor.replaced(v);
                            visitor.exit();
                        }
                        KvDiff::Diff(d) => {
                            visitor.enter(Enter::MapKey(k.to_string()));
                            d.accept(visitor);
                            visitor.exit();
                        }
                    }
                }
            }
        }
    };
}

kv_map_impl!(HashMap, Hash);
kv_map_impl!(BTreeMap, Ord);

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::tests::*;
    use crate::{Diffable, Replace};

    impl<'a> AcceptVisitor for ParentDiff<'a> {
        fn accept<V: Visitor>(&self, visitor: &mut V) {
            if !self.c1.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "c1",
                    serde_rename: None,
                });
                self.c1.accept(visitor);
                visitor.exit();
            }
            if !self.c2.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "c2",
                    serde_rename: None,
                });
                self.c2.accept(visitor);
                visitor.exit();
            }
            if !self.c3.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "c3",
                    serde_rename: None,
                });
                self.c3.accept(visitor);
                visitor.exit();
            }
            if !self.val.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "val",
                    serde_rename: None,
                });
                self.val.accept(visitor);
                visitor.exit();
            }
        }
    }

    impl<'a> AcceptVisitor for Child1Diff<'a> {
        fn accept<V: Visitor>(&self, visitor: &mut V) {
            if !self.x.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "x",
                    serde_rename: None,
                });
                self.x.accept(visitor);
                visitor.exit();
            }
            if !self.y.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "y",
                    serde_rename: None,
                });
                self.y.accept(visitor);
                visitor.exit();
            }
        }
    }

    impl<'a> AcceptVisitor for Child2Diff<'a> {
        fn accept<V: Visitor>(&self, visitor: &mut V) {
            if !self.a.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "a",
                    serde_rename: None,
                });
                self.a.accept(visitor);
                visitor.exit();
            }
            if !self.b.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "b",
                    serde_rename: None,
                });
                self.b.accept(visitor);
                visitor.exit();
            }
            if !self.c.is_unchanged() {
                visitor.enter(Enter::NamedField {
                    name: "c",
                    serde_rename: None,
                });
                self.c.accept(visitor);
                visitor.exit();
            }
        }
    }

    impl<'a> AcceptVisitor for SomeChildDiff<'a> {
        fn accept<V: Visitor>(&self, visitor: &mut V) {
            match self {
                SomeChildDiff::C1(c) => {
                    if !c.is_unchanged() {
                        visitor.enter(Enter::Variant {
                            name: "C1",
                            serde_rename: None,
                            serde_tag: SerdeVariantTag::External,
                        });
                        c.accept(visitor);
                        visitor.exit();
                    }
                }
                SomeChildDiff::C2(c) => {
                    if !c.is_unchanged() {
                        visitor.enter(Enter::Variant {
                            name: "C2",
                            serde_rename: None,
                            serde_tag: SerdeVariantTag::External,
                        });
                        c.accept(visitor);
                        visitor.exit();
                    }
                }
            }
        }
    }

    #[derive(Default, Clone, Debug, PartialEq)]
    struct Splice {
        from_index: usize,
        replace_count: usize,
        values: Vec<String>,
    }

    #[derive(Default, Clone, Debug)]
    pub struct Emitter {
        location: Vec<String>,
        replaced_locs: Vec<(String, String)>,
        spliced_locs: Vec<(String, Splice)>,
    }

    impl Visitor for Emitter {
        fn replaced<T: serde::Serialize>(&mut self, val: T) {
            let loc = self.location.join(".");
            if let Ok(val) = serde_json::to_string(&val) {
                self.replaced_locs.push((loc, val));
            };
        }

        fn splice<T: serde::Serialize>(&mut self, from_index: usize, replace: usize, values: &[T]) {
            let loc = self.location.join(".");
            if let Ok(values) = values
                .iter()
                .map(|v| serde_json::to_string(v))
                .collect::<Result<Vec<_>, _>>()
            {
                self.spliced_locs.push((
                    loc,
                    Splice {
                        from_index,
                        replace_count: replace,
                        values,
                    },
                ));
            };
        }

        fn enter(&mut self, val: Enter) {
            match val {
                Enter::NamedField { name, .. } => self.location.push(name.into()),
                Enter::PositionalField(p) => self.location.push(p.to_string()),
                Enter::Variant { name, .. } => self.location.push(name.into()),
                Enter::MapKey(k) => self.location.push(k),
                Enter::Index(ix) => self.location.push(ix.to_string()),
            }
        }

        fn exit(&mut self) {
            self.location.pop().unwrap();
        }
    }

    impl Emitter {
        fn expect_locs(&self, expect: impl IntoIterator<Item = (&'static str, &'static str)>) {
            for ((loc, val), (expect_loc, expect_val)) in
                self.replaced_locs.iter().zip(expect.into_iter())
            {
                assert_eq!(loc, expect_loc);
                assert_eq!(val, expect_val);
            }
        }

        fn expect_splice(&self, expect: impl IntoIterator<Item = (&'static str, Splice)>) {
            for ((loc, splice), (expect_loc, expect_splice)) in
                self.spliced_locs.iter().zip(expect.into_iter())
            {
                assert_eq!(loc, expect_loc);
                assert_eq!(splice, &expect_splice);
            }
        }
    }

    #[test]
    fn test_visitor() {
        let p1 = Parent {
            c1: Child1 {
                x: 123,
                y: "hi".into(),
            },
            c2: vec![
                Child1 {
                    x: 1,
                    y: "y1".into(),
                },
                Child1 {
                    x: 2,
                    y: "y2".into(),
                },
            ],
            c3: HashMap::new(),
            val: "val".into(),
        };

        let mut p2 = p1.clone();
        p2.c1.x = 234;
        p2.val = "new val".into();
        p2.c2[0].y = "y1x".into();
        p2.c2.pop().unwrap();
        p2.c2.push(Child1 {
            x: 333,
            y: "y3".into(),
        });

        let mut emitter = Emitter::default();
        let diff = p1.diff(&p2);
        diff.accept(&mut emitter);

        let expect = [("c1.x", "234"), ("c2.0.y", "\"y1x\"")];

        emitter.expect_locs(expect);
        emitter.expect_splice([(
            "c2",
            Splice {
                from_index: 1,
                replace_count: 1,
                values: vec![r#"{"x":333,"y":"y3"}"#.into()],
            },
        )]);
    }

    #[test]
    fn test_kv_visitor() {
        let map1: BTreeMap<_, _> = [("x", 123), ("y", 234)].into_iter().collect();
        let map2: BTreeMap<_, _> = [("x", 321), ("y", 234), ("z", 345)].into_iter().collect();
        let diff = map1.diff(&map2);

        let mut emitter = Emitter::default();
        diff.accept(&mut emitter);

        let expect = [("x", "321"), ("z", "345")];
        emitter.expect_locs(expect);
    }
}

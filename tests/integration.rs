#![deny(warnings)]

// *** Impls ***

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
struct SimpleStruct {
    x: String,
    y: i32,
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
struct StrangeStruct {
    #[expect(clippy::type_complexity, reason = "test")]
    r#try: Option<Box<(u32, (&'static str, Box<u64>))>>,
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
struct Unit;

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
struct Tuple(Vec<&'static str>, i32);

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
enum SimpleEnum {
    First,
    Second(i32),
    Third { x: String, y: () },
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
struct StructContainsAtomicAndSkip {
    #[diffable(atomic)]
    x: Vec<SimpleStruct>,
    y: i32,
}

#[derive(PartialEq, Debug, Clone)]
enum NotDiffable {
    X,
    Y,
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
enum EnumContainsAtomicAndSkip {
    First,
    Second(#[diffable(atomic)] Vec<Unit>, i32),
    Third {
        #[diffable(atomic)]
        w: Vec<Unit>,
        x: i32,
        #[diffable(skip)]
        y: NotDiffable,
        z: u32,
    },
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
struct SingleFieldA {
    a: SingleFieldB,
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
struct SingleFieldB {
    b: SingleFieldC,
}

#[derive(difficient::Diffable, PartialEq, Debug, Clone)]
struct SingleFieldC {
    c: u32,
}

#[expect(dead_code, reason = "test")]
#[expect(
    clippy::empty_structs_with_brackets,
    reason = "We are specifically testing this behavior"
)]
mod just_check_they_compile {
    use std::collections::{BTreeMap, HashMap};

    use crate::{NotDiffable, SimpleStruct};

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    struct EmptyStruct {}

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    struct EmptyTupleStruct();

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    enum FieldlessEnum {
        A,
    }

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    enum AnotherFieldlessEnum {
        A,
        B(),
        C {},
    }

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    #[expect(
        non_camel_case_types,
        non_snake_case,
        clippy::upper_case_acronyms,
        reason = "test"
    )]
    enum dumb_Enum_noWarnings {
        lowercase { UPPERCASE: i32 },
        UPPERCASE { lowercase: i32 },
    }

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    #[expect(non_snake_case, non_camel_case_types, reason = "test")]
    struct dumb_Struct_noWarnings {
        UPPERCASE: i32,
        camelCase: i32,
    }

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    struct HasHashMap {
        map1: HashMap<i32, i32>,
        map2: BTreeMap<i32, i32>,
    }

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    struct TupleStructContainsAtomicAndSkip(
        #[diffable(atomic)] Vec<SimpleStruct>,
        #[diffable(skip)] i32,
        u32,
    );

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    #[diffable(atomic)]
    struct AtomicStruct {
        x: NotDiffable,
        y: NotDiffable,
    }

    #[derive(difficient::Diffable, PartialEq, Debug, Clone)]
    #[diffable(visit_transparent)]
    struct SeeThrough {
        x: u32,
    }
}

// **** Derive tests
mod tests {
    use difficient::{AtomicDiff, DiffKey, Diffable, PatchOnlyDiff};
    use pretty_assertions::assert_eq;
    use rand::Rng;

    use super::*;

    #[test]
    fn test_simple_struct() {
        let mut it1 = SimpleStruct {
            x: "hello".into(),
            y: 123,
        };
        let it2 = SimpleStruct {
            x: "bye".into(),
            y: 123,
        };
        let diff = it1.diff(&it2);
        it1.apply(&diff).unwrap();
        assert_eq!(it1, it2);
    }

    #[test]
    fn test_less_simple_struct() {
        let mut it1 = StrangeStruct {
            r#try: Some(Box::new((123, ("ick", Box::new(543))))),
        };
        let it2 = StrangeStruct {
            r#try: Some(Box::new((123, ("flick", Box::new(543))))),
        };
        let diff = it1.diff(&it2);
        it1.apply(&diff).unwrap();
        assert_eq!(it1, it2);
    }

    #[test]
    fn test_unit_struct() {
        let mut it1 = Unit;
        let it2 = Unit;
        let diff = it1.diff(&it2);
        it1.apply(&diff).unwrap();
        assert_eq!(it1, it2);
    }

    #[test]
    fn test_tuple_struct() {
        let mut it1 = Tuple(vec!["first", "second"], 123);
        let it2 = Tuple(vec!["second", "third"], 123);
        let diff = it1.diff(&it2);
        it1.apply(&diff).unwrap();
        assert_eq!(it1, it2);
    }

    #[test]
    fn test_simple_enum() {
        let mut it1 = SimpleEnum::First;
        let mut it2 = SimpleEnum::Second(123);
        let mut it3 = SimpleEnum::Third {
            x: "work work".into(),
            y: (),
        };
        let it4 = SimpleEnum::Third {
            x: "twork".into(),
            y: (),
        };

        {
            let diff = it1.diff(&it2);
            it1.apply(&diff).unwrap();
            assert_eq!(it1, it2);
        }

        {
            let diff = it2.diff(&it3);
            it2.apply(&diff).unwrap();
            assert_eq!(it2, it3);
        }

        {
            let diff = it3.diff(&it4);
            it3.apply(&diff).unwrap();
            assert_eq!(it3, it4);
        }
    }

    #[test]
    fn test_atomic_struct() {
        let mut c1 = StructContainsAtomicAndSkip { x: vec![], y: 234 };
        let c2 = StructContainsAtomicAndSkip {
            x: vec![SimpleStruct {
                x: "s".into(),
                y: 123,
            }],
            y: 234,
        };
        let diff = c1.diff(&c2);
        c1.apply(&diff).unwrap();
        assert_eq!(c1, c2);
    }

    #[test]
    fn test_atomic_and_skip_enum() {
        // we are really just checking things compile and run here
        let mut c1 = EnumContainsAtomicAndSkip::First;
        let c2a = EnumContainsAtomicAndSkip::Second(Vec::new(), 123);
        let c2b = EnumContainsAtomicAndSkip::Second(vec![Unit, Unit], 123);
        let c3a = EnumContainsAtomicAndSkip::Third {
            w: vec![Unit],
            x: 321,
            y: NotDiffable::X,
            z: 654,
        };
        let c3b = EnumContainsAtomicAndSkip::Third {
            w: vec![Unit, Unit, Unit],
            x: 321,
            y: NotDiffable::Y,
            z: 987,
        };
        for item in &[c2a, c2b, c3a] {
            let diff = c1.diff(item);
            c1.apply(&diff).unwrap();
            assert_eq!(&c1, item);
        }

        // c3b is special due to NotDiffable
        let diff = c1.diff(&c3b);
        c1.apply(&diff).unwrap();
        assert_eq!(
            c1,
            EnumContainsAtomicAndSkip::Third {
                w: vec![Unit, Unit, Unit],
                x: 321,
                y: NotDiffable::X, // has not changed from c3a
                z: 987
            }
        )
    }

    #[derive(Clone, Diffable, PartialEq, Eq)]
    struct Pair {
        key: i32,
        val: String,
    }

    impl std::fmt::Debug for Pair {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{{ key: {}, val: {} }}", self.key, self.val)
        }
    }

    impl DiffKey for Pair {
        type Key<'a> = i32;

        fn key(&self) -> Self::Key<'_> {
            self.key
        }
    }

    impl Pair {
        fn new(key: i32, val: &'static str) -> Self {
            Self {
                key,
                val: val.into(),
            }
        }
    }

    #[test]
    fn test_compound_vec() {
        let left = vec![Pair::new(1, "a"), Pair::new(2, "b")];

        {
            let mut left = left.clone();
            let right = vec![Pair::new(1, "z"), Pair::new(2, "b")];
            let diff = left.diff(&right);
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            let mut left = left.clone();
            let right = vec![Pair::new(2, "c"), Pair::new(2, "b")];
            let diff = left.diff(&right);
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            let mut left = left.clone();
            let right = vec![Pair::new(1, "a"), Pair::new(2, "d"), Pair::new(3, "c")];
            let diff = left.diff(&right);
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }
    }

    #[test]
    fn awkward_change() {
        let mut left = vec![
            Pair::new(0, "a"),
            Pair::new(1, "a"),
            Pair::new(2, "a"),
            Pair::new(0, "a"),
            Pair::new(4, "a"),
        ];

        let right = vec![
            Pair::new(0, "b"),
            Pair::new(4, "a"),
            Pair::new(0, "a"),
            Pair::new(3, "b"),
            Pair::new(4, "a"),
        ];

        let diff = left.diff(&right);
        left.apply(&diff).unwrap();
        assert_eq!(left, right);
    }

    #[test]
    fn fuzz_vec_impl() {
        let mut rng = rand::thread_rng();

        for _iter in 0..100 {
            let left_len = rng.gen_range(0..100);
            let right_len = rng.gen_range(0..100);
            let mut left: Vec<Pair> = (0..left_len)
                .map(|ix| {
                    let key = if rng.gen_bool(0.8) {
                        ix
                    } else {
                        rng.gen_range(0..left_len)
                    };
                    let val = if rng.gen_bool(0.7) { "a" } else { "b" };
                    Pair::new(key, val)
                })
                .collect();
            let right: Vec<Pair> = (0..right_len)
                .map(|ix| {
                    let key = if rng.gen_bool(0.8) {
                        ix
                    } else {
                        rng.gen_range(0..right_len)
                    };
                    let val = if rng.gen_bool(0.8) { "a" } else { "b" };
                    Pair::new(key, val)
                })
                .collect();
            let diff = left.diff(&right);
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }
    }

    #[test]
    fn test_nested_single_field_structs_gives_targeted_patch() {
        let mut left = SingleFieldA {
            a: SingleFieldB {
                b: SingleFieldC { c: 1 },
            },
        };
        let right = SingleFieldA {
            a: SingleFieldB {
                b: SingleFieldC { c: 2 },
            },
        };
        let diff = left.diff(&right);
        assert_eq!(
            diff,
            PatchOnlyDiff::Patched(SingleFieldADiff {
                a: PatchOnlyDiff::Patched(SingleFieldBDiff {
                    b: PatchOnlyDiff::Patched(SingleFieldCDiff {
                        c: AtomicDiff::Replaced(&2)
                    })
                })
            })
        );
        left.apply(&diff).unwrap();
        assert_eq!(left, right);
    }
}

#[cfg(feature = "serde")]
mod serde_test {
    use difficient::Diffable;

    use super::*;

    #[test]
    fn test_serializable() {
        let s1 = SimpleEnum::Third {
            x: "hi".into(),
            y: (),
        };
        let s2 = SimpleEnum::Third {
            x: "bye".into(),
            y: (),
        };
        let patch = s1.diff(&s2);
        let json = serde_json::to_string_pretty(&patch).unwrap();

        // TODO this isn't very pretty...
        let expect = r#"{
  "Patched": {
    "Third": {
      "x": {
        "Replaced": "bye"
      },
      "y": null
    }
  }
}"#;
        assert_eq!(json, expect);
    }
}

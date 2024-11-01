// We chose to use `lcs` over `myers` because it can result in smaller changes
// when rearranging concepts.
//
// When a concept is rearranged, both its `z_index` and its position in the
// `concepts` vector changes. Ideally, we would minimize the resulting changes
// such that we remove the rearranged concept from the vector, and reinsert it
// with its updated `z_index` in the new position. Unfortunately, sometimes the
// diffing algorithm decides upon a different set of changes: It would remove
// a non-rearranged concept and place it into a new position to swap places with
// the rearranged one. And then it also needs a separate `Patch` to update the
// `z_index` of the concept that was actually rearranged. This behavior still
// happens sometimes, but is minimized by using `lcs` instead of `myers`.
use similar::algorithms::lcs as diff_algo;

use similar::algorithms::{Capture, Compact, Replace as SimilarReplace};

use crate::{Apply, ApplyError, Diffable, Replace};

/// A trait that must be implemented by any type `T` that is part of a
/// `Vec<T>` being diffed.
///
/// The value for Key should be unique to that element in the list (akin to React in JS land).
/// It allows the diff algorithm to detect whether elements
/// have been changed, moved or completely replaced.
///
/// If there is no sensible value to use as the key, consider making a dummy ID
/// to attach to T. Otherwise, it is acceptable to use a null value like `()`, but
/// this may cause the diff algorithm to run slowly and generate unnecessarily large diffs
pub trait DiffKey {
    type Key<'a>: PartialEq
    where
        Self: 'a;
    fn key(&self) -> Self::Key<'_>;
}

macro_rules! impl_diffkey_for_primitives{
        ($($typ: ty)*) => ($(
            impl DiffKey for $typ {
                type Key<'a> = Self;

                fn key(&self) -> Self::Key<'_> {
                    *self
                }
            }
        )*);
    }

impl_diffkey_for_primitives! {
    i8 i16 i32 i64
    u8 u16 u32 u64
    f32 f64
    bool
    &'static str
}

impl DiffKey for String {
    type Key<'a> = &'a Self;

    fn key(&self) -> Self::Key<'_> {
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "visitor", derive(serde::Serialize))]
pub enum VecDiff<'a, T, U> {
    Unchanged,
    Replaced(&'a [T]),
    Changed { changes: Vec<VecChange<'a, T, U>> },
}

#[cfg(feature = "visitor")]
mod serde_impls {
    use crate::{AcceptVisitor, Enter, VecChange, VecDiff};

    impl<'a, T, U> AcceptVisitor for VecDiff<'a, T, U>
    where
        T: serde::Serialize,
        U: AcceptVisitor,
    {
        fn accept<V: crate::Visitor>(&self, visitor: &mut V) {
            match self {
                Self::Unchanged => {}
                Self::Changed { changes } => {
                    for chg in changes {
                        chg.accept(visitor)
                    }
                }
                Self::Replaced(r) => visitor.replaced(r),
            }
        }
    }
    impl<'a, T, U> AcceptVisitor for VecChange<'a, T, U>
    where
        T: serde::Serialize,
        U: AcceptVisitor,
    {
        fn accept<V: crate::Visitor>(&self, visitor: &mut V) {
            match self {
                VecChange::Remove { at_index, count } => {
                    visitor.splice::<T>(*at_index, *count, &[])
                }
                VecChange::Insert { at_index, values } => visitor.splice(*at_index, 0, values),
                VecChange::Splice {
                    at_index,
                    count,
                    values,
                } => visitor.splice::<T>(*at_index, *count, values),
                VecChange::Patch { at_index, patch } => {
                    visitor.enter(Enter::Index(*at_index));
                    patch.accept(visitor);
                    visitor.exit();
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum VecChange<'a, T, U> {
    Remove {
        at_index: usize,
        count: usize,
    },
    Insert {
        at_index: usize,
        values: &'a [T],
    },
    Splice {
        at_index: usize,
        count: usize,
        values: &'a [T],
    },
    Patch {
        at_index: usize,
        patch: U,
    },
}

impl<'a, T> Apply for VecDiff<'a, T, T::Diff>
where
    T: Diffable<'a> + Clone,
{
    type Parent = Vec<T>;

    fn apply_to_base(&self, source: &mut Self::Parent, errs: &mut Vec<ApplyError>) {
        match self {
            VecDiff::Unchanged => {}
            VecDiff::Replaced(slice) => *source = slice.to_vec(),
            VecDiff::Changed { changes } => {
                for change in changes.iter() {
                    match change {
                        VecChange::Remove {
                            at_index: from_index,
                            count,
                        } => {
                            source.splice(*from_index..*from_index + count, None);
                        }
                        VecChange::Insert { at_index, values } => {
                            source.splice(*at_index..*at_index, values.iter().cloned());
                        }
                        VecChange::Splice {
                            at_index,
                            count,
                            values,
                        } => {
                            source.splice(*at_index..*at_index + count, values.iter().cloned());
                        }
                        VecChange::Patch { at_index, patch } => {
                            patch.apply_to_base(&mut source[*at_index], errs);
                        }
                    }
                }
            }
        }
    }
}

impl<'a, T> Replace for VecDiff<'a, T, T::Diff>
where
    T: Diffable<'a>,
{
    fn is_unchanged(&self) -> bool {
        matches!(self, Self::Unchanged)
    }

    fn is_replaced(&self) -> bool {
        matches!(self, Self::Replaced(_))
    }
}

impl<'a, T> Diffable<'a> for Vec<T>
where
    T: 'a + Clone + PartialEq + DiffKey + Diffable<'a>,
{
    type Diff = VecDiff<'a, T, T::Diff>;

    fn diff(&self, other: &'a Self) -> Self::Diff {
        if self.is_empty() && other.is_empty() {
            // short circuit
            return VecDiff::Unchanged;
        }
        let mut changes = Vec::new();
        let lkeys: Vec<_> = self.iter().map(|v| v.key()).collect();
        let rkeys: Vec<_> = other.iter().map(|v| v.key()).collect();
        let mut d = Compact::new(SimilarReplace::new(Capture::new()), &lkeys, &rkeys);
        diff_algo::diff(&mut d, &lkeys, 0..lkeys.len(), &rkeys, 0..rkeys.len()).unwrap();
        let ops = d.into_inner().into_inner().into_ops();
        let n_ops = ops.len();
        let mut offset = 0isize;
        for op in ops {
            match op {
                similar::DiffOp::Equal {
                    old_index,
                    new_index,
                    len,
                } => {
                    assert!(len > 0);
                    for ix in 0..len {
                        let left = &self[old_index + ix];
                        let right = &other[new_index + ix];
                        let diff = left.diff(right);
                        if diff.is_unchanged() {
                            continue;
                        }
                        changes.push(VecChange::Patch {
                            at_index: new_index + ix,
                            patch: diff,
                        })
                    }
                }
                similar::DiffOp::Delete {
                    old_len, old_index, ..
                } => {
                    assert!(old_len > 0);
                    changes.push(VecChange::Remove {
                        at_index: (old_index as isize + offset) as usize,
                        count: old_len,
                    });
                    offset -= old_len as isize;
                }
                similar::DiffOp::Insert {
                    new_index, new_len, ..
                } => {
                    assert!(new_len > 0);
                    changes.push(VecChange::Insert {
                        at_index: new_index,
                        values: &other[new_index..new_index + new_len],
                    });
                    offset += new_len as isize;
                }
                similar::DiffOp::Replace {
                    old_index,
                    old_len,
                    new_index,
                    new_len,
                    ..
                } => {
                    assert!(old_len + new_len > 0);
                    if old_len == self.len() {
                        assert_eq!(n_ops, 1);
                        // everything has been replaced!
                        return VecDiff::Replaced(other);
                    }
                    changes.push(VecChange::Splice {
                        at_index: (old_index as isize + offset) as usize,
                        count: old_len,
                        values: &other[new_index..new_index + new_len],
                    });
                    offset -= old_len as isize;
                    offset += new_len as isize;
                }
            }
        }
        if changes.is_empty() {
            VecDiff::Unchanged
        } else {
            VecDiff::Changed { changes }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null_vec() {
        let empty: Vec<i32> = Vec::new();
        let diff = empty.diff(&empty);
        assert_eq!(diff, VecDiff::Unchanged);
    }

    #[test]
    fn test_simple_vec() {
        let left = vec![1, 2, 3, 4, 5];
        {
            // no-op
            let diff = left.diff(&left);
            assert_eq!(diff, VecDiff::Unchanged);
        }

        {
            // replace one
            let mut left = left.clone();
            let right = vec![1, 2, 3, 4, 6];
            let diff = left.diff(&right);
            assert_eq!(
                diff,
                VecDiff::Changed {
                    changes: vec![VecChange::Splice {
                        at_index: 4,
                        count: 1,
                        values: &[6]
                    }]
                }
            );
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            // append two
            let mut left = left.clone();
            let right = vec![1, 2, 3, 4, 5, 6, 7];
            let diff = left.diff(&right);
            assert_eq!(
                diff,
                VecDiff::Changed {
                    changes: vec![VecChange::Insert {
                        at_index: 5,
                        values: &[6, 7]
                    }]
                }
            );
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            // insert two
            let mut left = left.clone();
            let right = vec![1, 2, 3, 4, 6, 7, 5];
            let diff = left.diff(&right);
            assert_eq!(
                diff,
                VecDiff::Changed {
                    changes: vec![VecChange::Insert {
                        at_index: 4,
                        values: &[6, 7]
                    }]
                }
            );
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            // remove and insert
            let mut left = left.clone();
            let right = vec![3, 4, 5, 6, 7];
            let diff = left.diff(&right);
            assert_eq!(
                diff,
                VecDiff::Changed {
                    changes: vec![
                        VecChange::Remove {
                            at_index: 0,
                            count: 2,
                        },
                        VecChange::Insert {
                            at_index: 3,
                            values: &[6, 7]
                        }
                    ]
                }
            );
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            // replace all
            let mut left = left.clone();
            let right = vec![6, 7, 8, 9, 10];
            let diff = left.diff(&right);
            assert_eq!(diff, VecDiff::Replaced(&right));
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }

        {
            // remove all
            let mut left = left.clone();
            let right = vec![];
            let diff = left.diff(&right);
            assert_eq!(
                diff,
                VecDiff::Changed {
                    changes: vec![VecChange::Remove {
                        at_index: 0,
                        count: 5
                    }]
                }
            );
            // TODO should instead be VecDiff::Replaced(&right)??
            left.apply(&diff).unwrap();
            assert_eq!(left, right);
        }
    }

    #[test]
    fn awkward_1() {
        let mut left = vec![0, 1, 2, 3, 4];
        let right = vec![0, 1, 4, 1, 4];
        let diff = left.diff(&right);
        left.apply(&diff).unwrap();
        assert_eq!(left, right);
    }

    #[test]
    fn awkward_2() {
        let mut left = vec![1, 1, 2];
        let right = vec![0, 1, 2, 3, 1, 1, 2];
        let diff = left.diff(&right);
        left.apply(&diff).unwrap();
        assert_eq!(left, right);
    }
}

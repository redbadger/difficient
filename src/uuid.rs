use uuid::Uuid;

use crate::{AtomicDiff, DiffKey, Diffable};

impl<'a> Diffable<'a> for Uuid {
    type Diff = AtomicDiff<'a, Self>;

    fn diff(&self, other: &'a Self) -> Self::Diff {
        if self == other {
            AtomicDiff::Unchanged
        } else {
            AtomicDiff::Replaced(other)
        }
    }
}

impl DiffKey for Uuid {
    type Key<'a> = Uuid;

    fn key(&self) -> Self::Key<'_> {
        *self
    }
}

use crate::{AtomicDiff, Diffable};
use chrono::DateTime;

impl<'a, Tz: chrono::TimeZone + 'a> Diffable<'a> for DateTime<Tz> {
    type Diff = AtomicDiff<'a, Self>;

    fn diff(&self, other: &'a Self) -> Self::Diff {
        if self == other {
            AtomicDiff::Unchanged
        } else {
            AtomicDiff::Replaced(other)
        }
    }
}

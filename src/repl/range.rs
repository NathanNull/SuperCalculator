use std::ops::{
    Range as RangeBase, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive,
};

#[derive(Clone)]
pub enum Range {
    Base(RangeBase<usize>),
    Inclusive(RangeInclusive<usize>),
    From(RangeFrom<usize>),
    To(RangeTo<usize>),
    ToInclusive(RangeToInclusive<usize>),
    Full(RangeFull),
}

impl Range {
    pub fn contains(&self, val: usize) -> bool {
        match self {
            Range::Base(range) => range.contains(&val),
            Range::Inclusive(range) => range.contains(&val),
            Range::From(range) => range.contains(&val),
            Range::To(range) => range.contains(&val),
            Range::ToInclusive(range) => range.contains(&val),
            Range::Full(_) => true,
        }
    }

    pub fn min(&self) -> usize {
        match self {
            Range::Base(range) => range.start,
            Range::Inclusive(range) => *range.start(),
            Range::From(range) => range.start,
            Range::To(_) => 0,
            Range::ToInclusive(_) => 0,
            Range::Full(_) => 0,
        }
    }

    pub fn max(&self) -> usize {
        match self {
            Range::Base(range) => range.end - 1,
            Range::Inclusive(range) => *range.end(),
            Range::From(_) => usize::MAX,
            Range::To(range) => range.end - 1,
            Range::ToInclusive(range) => range.end,
            Range::Full(_) => usize::MAX,
        }
    }

    pub fn shift(&self, val: isize) -> Self {
        (self.min().saturating_add_signed(val)..=self.max().saturating_add_signed(val)).into()
    }
}

impl From<RangeBase<usize>> for Range {
    fn from(value: RangeBase<usize>) -> Self {
        Self::Base(value)
    }
}
impl From<RangeInclusive<usize>> for Range {
    fn from(value: RangeInclusive<usize>) -> Self {
        Self::Inclusive(value)
    }
}
impl From<RangeFrom<usize>> for Range {
    fn from(value: RangeFrom<usize>) -> Self {
        Self::From(value)
    }
}

impl From<RangeTo<usize>> for Range {
    fn from(value: RangeTo<usize>) -> Self {
        Self::To(value)
    }
}
impl From<RangeToInclusive<usize>> for Range {
    fn from(value: RangeToInclusive<usize>) -> Self {
        Self::ToInclusive(value)
    }
}
impl From<RangeFull> for Range {
    fn from(value: RangeFull) -> Self {
        Self::Full(value)
    }
}

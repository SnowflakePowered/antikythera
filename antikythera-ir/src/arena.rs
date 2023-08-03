// Lifted almost verbatim from https://github.com/gfx-rs/naga/blob/5f8e4f6dead465f43b9727f151fa773770d6b488/src/arena.rs.
use std::{cmp::Ordering, fmt, hash, marker::PhantomData, num::NonZeroU32, ops};

/// An unique index in the arena array that a handle points to.
/// The "non-zero" part ensures that an `Option<Handle<T>>` has
/// the same size and representation as `Handle<T>`.
type Index = NonZeroU32;

#[derive(Clone, Copy, Debug, thiserror::Error, PartialEq)]
#[error("Handle {index} of {kind} is either not present, or inaccessible yet")]
pub struct BadHandle {
    pub kind: &'static str,
    pub index: usize,
}

impl BadHandle {
    fn new<T>(handle: Handle<T>) -> Self {
        Self {
            kind: std::any::type_name::<T>(),
            index: handle.index(),
        }
    }
}

/// A strongly typed reference to an arena item.
///
/// A `Handle` value can be used as an index into an [`Arena`] or [`UniqueArena`].
#[derive(bincode::Decode, bincode::Encode)]
pub struct Handle<T> {
    index: Index,
    marker: PhantomData<T>,
}

impl<T> Clone for Handle<T> {
    fn clone(&self) -> Self {
        Handle {
            index: self.index,
            marker: self.marker,
        }
    }
}

impl<T> Copy for Handle<T> {}

impl<T> PartialEq for Handle<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> Eq for Handle<T> {}

impl<T> PartialOrd for Handle<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.index.partial_cmp(&other.index)
    }
}

impl<T> Ord for Handle<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

impl<T> fmt::Debug for Handle<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "[{}]", self.index)
    }
}

impl<T> hash::Hash for Handle<T> {
    fn hash<H: hash::Hasher>(&self, hasher: &mut H) {
        self.index.hash(hasher)
    }
}

impl<T> Handle<T> {
    #[cfg(test)]
    pub const DUMMY: Self = Handle {
        index: unsafe { NonZeroU32::new_unchecked(u32::MAX) },
        marker: PhantomData,
    };

    pub(crate) const fn new(index: Index) -> Self {
        Handle {
            index,
            marker: PhantomData,
        }
    }

    /// Returns the zero-based index of this handle.
    pub const fn index(self) -> usize {
        let index = self.index.get() - 1;
        index as usize
    }

    /// Convert a `usize` index into a `Handle<T>`.
    fn from_usize(index: usize) -> Self {
        let handle_index = u32::try_from(index + 1)
            .ok()
            .and_then(Index::new)
            .expect("Failed to insert into arena. Handle overflows");
        Handle::new(handle_index)
    }

    /// Convert a `usize` index into a `Handle<T>`, without range checks.
    const unsafe fn from_usize_unchecked(index: usize) -> Self {
        Handle::new(Index::new_unchecked((index + 1) as u32))
    }
}

/// A strongly typed range of handles.
#[cfg_attr(feature = "serialize", derive(serde::Serialize))]
#[cfg_attr(feature = "deserialize", derive(serde::Deserialize))]
#[cfg_attr(
    any(feature = "serialize", feature = "deserialize"),
    serde(transparent)
)]
#[cfg_attr(feature = "arbitrary", derive(arbitrary::Arbitrary))]
pub struct Range<T> {
    inner: ops::Range<u32>,
    #[cfg_attr(any(feature = "serialize", feature = "deserialize"), serde(skip))]
    marker: PhantomData<T>,
}

impl<T> Range<T> {
    pub(crate) const fn erase_type(self) -> Range<()> {
        let Self { inner, marker: _ } = self;
        Range {
            inner,
            marker: PhantomData,
        }
    }
}

// NOTE: Keep this diagnostic in sync with that of [`BadHandle`].
#[derive(Clone, Debug, thiserror::Error)]
#[error("Handle range {range:?} of {kind} is either not present, or inaccessible yet")]
pub struct BadRangeError {
    // This error is used for many `Handle` types, but there's no point in making this generic, so
    // we just flatten them all to `Handle<()>` here.
    kind: &'static str,
    range: Range<()>,
}

impl BadRangeError {
    pub fn new<T>(range: Range<T>) -> Self {
        Self {
            kind: std::any::type_name::<T>(),
            range: range.erase_type(),
        }
    }
}

impl<T> Clone for Range<T> {
    fn clone(&self) -> Self {
        Range {
            inner: self.inner.clone(),
            marker: self.marker,
        }
    }
}

impl<T> fmt::Debug for Range<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "[{}..{}]", self.inner.start + 1, self.inner.end)
    }
}

impl<T> Iterator for Range<T> {
    type Item = Handle<T>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.inner.start < self.inner.end {
            self.inner.start += 1;
            Some(Handle {
                index: NonZeroU32::new(self.inner.start).unwrap(),
                marker: self.marker,
            })
        } else {
            None
        }
    }
}

impl<T> Range<T> {
    pub fn new_from_bounds(first: Handle<T>, last: Handle<T>) -> Self {
        Self {
            inner: (first.index() as u32)..(last.index() as u32 + 1),
            marker: Default::default(),
        }
    }
}

/// An arena holding some kind of component (e.g., type, constant,
/// instruction, etc.) that can be referenced.
///
/// Adding new items to the arena produces a strongly-typed [`Handle`].
/// The arena can be indexed using the given handle to obtain
/// a reference to the stored item.
#[cfg_attr(test, derive(PartialEq))]
#[derive(Clone, bincode::Encode, bincode::Decode)]
pub struct Arena<T: 'static> {
    /// Values of this arena.
    data: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T: Default> Arena<T> {
    /// Adds a new value to the arena using the given factory, passing in the
    /// handle that is created.
    pub fn append_recursive(&mut self, value: impl FnOnce(&mut Self, Handle<T>) -> T) -> Handle<T> {
        let index = self.data.len();
        self.data.push(T::default()); // make dummy value
        let handle = Handle::from_usize(index);
        let value = value(self, handle.clone());

        // replace with actual value
        self.data[index] = value;

        handle
    }
}

impl<T> Arena<T> {
    /// Create a new arena with no initial capacity allocated.
    pub const fn new() -> Self {
        Arena { data: Vec::new() }
    }

    /// Extracts the inner vector.
    #[allow(clippy::missing_const_for_fn)] // ignore due to requirement of #![feature(const_precise_live_drops)]
    pub fn into_inner(self) -> Vec<T> {
        self.data
    }

    /// Returns the current number of items stored in this arena.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the arena contains no elements.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns an iterator over the items stored in this arena, returning both
    /// the item's handle and a reference to it.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = (Handle<T>, &T)> {
        self.data
            .iter()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Returns a iterator over the items stored in this arena,
    /// returning both the item's handle and a mutable reference to it.
    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = (Handle<T>, &mut T)> {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(i, v)| unsafe { (Handle::from_usize_unchecked(i), v) })
    }

    /// Adds a new value to the arena, returning a typed handle.
    pub fn append(&mut self, value: T) -> Handle<T> {
        let index = self.data.len();
        self.data.push(value);
        Handle::from_usize(index)
    }

    /// Fetch a handle to an existing type.
    pub fn fetch_if<F: Fn(&T) -> bool>(&self, fun: F) -> Option<Handle<T>> {
        self.data
            .iter()
            .position(fun)
            .map(|index| unsafe { Handle::from_usize_unchecked(index) })
    }

    /// Adds a value with a custom check for uniqueness:
    /// returns a handle pointing to
    /// an existing element if the check succeeds, or adds a new
    /// element otherwise.
    pub fn fetch_if_or_append<F: Fn(&T, &T) -> bool>(&mut self, value: T, fun: F) -> Handle<T> {
        if let Some(index) = self.data.iter().position(|d| fun(d, &value)) {
            unsafe { Handle::from_usize_unchecked(index) }
        } else {
            self.append(value)
        }
    }

    /// Adds a value with a check for uniqueness, where the check is plain comparison.
    pub fn fetch_or_append(&mut self, value: T) -> Handle<T>
    where
        T: PartialEq,
    {
        self.fetch_if_or_append(value, T::eq)
    }

    pub fn try_get(&self, handle: Handle<T>) -> Result<&T, BadHandle> {
        self.data
            .get(handle.index())
            .ok_or_else(|| BadHandle::new(handle))
    }

    /// Get a mutable reference to an element in the arena.
    pub fn get_mut(&mut self, handle: Handle<T>) -> &mut T {
        self.data.get_mut(handle.index()).unwrap()
    }

    /// Get the range of handles from a particular number of elements to the end.
    pub fn range_from(&self, old_length: usize) -> Range<T> {
        Range {
            inner: old_length as u32..self.data.len() as u32,
            marker: PhantomData,
        }
    }

    /// Clears the arena keeping all allocations
    pub fn clear(&mut self) {
        self.data.clear()
    }

    /// Assert that `handle` is valid for this arena.
    pub fn check_contains_handle(&self, handle: Handle<T>) -> Result<(), BadHandle> {
        if handle.index() < self.data.len() {
            Ok(())
        } else {
            Err(BadHandle::new(handle))
        }
    }

    /// Assert that `range` is valid for this arena.
    pub fn check_contains_range(&self, range: &Range<T>) -> Result<(), BadRangeError> {
        // Since `range.inner` is a `Range<u32>`, we only need to
        // check that the start precedes the end, and that the end is
        // in range.
        if range.inner.start > range.inner.end
            || self
                .check_contains_handle(Handle::new(range.inner.end.try_into().unwrap()))
                .is_err()
        {
            Err(BadRangeError::new(range.clone()))
        } else {
            Ok(())
        }
    }
}

impl<T> ops::Index<Handle<T>> for Arena<T> {
    type Output = T;
    fn index(&self, handle: Handle<T>) -> &T {
        &self.data[handle.index()]
    }
}

impl<T> ops::IndexMut<Handle<T>> for Arena<T> {
    fn index_mut(&mut self, handle: Handle<T>) -> &mut T {
        &mut self.data[handle.index()]
    }
}

impl<T> ops::Index<Range<T>> for Arena<T> {
    type Output = [T];
    fn index(&self, range: Range<T>) -> &[T] {
        &self.data[range.inner.start as usize..range.inner.end as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn append_non_unique() {
        let mut arena: Arena<u8> = Arena::new();
        let t1 = arena.append(0);
        let t2 = arena.append(0);
        assert!(t1 != t2);
        assert!(arena[t1] == arena[t2]);
    }

    #[test]
    fn append_unique() {
        let mut arena: Arena<u8> = Arena::new();
        let t1 = arena.append(0);
        let t2 = arena.append(1);
        assert!(t1 != t2);
        assert!(arena[t1] != arena[t2]);
    }

    #[test]
    fn fetch_or_append_non_unique() {
        let mut arena: Arena<u8> = Arena::new();
        let t1 = arena.fetch_or_append(0);
        let t2 = arena.fetch_or_append(0);
        assert!(t1 == t2);
        assert!(arena[t1] == arena[t2])
    }

    #[test]
    fn fetch_or_append_unique() {
        let mut arena: Arena<u8> = Arena::new();
        let t1 = arena.fetch_or_append(0);
        let t2 = arena.fetch_or_append(1);
        assert!(t1 != t2);
        assert!(arena[t1] != arena[t2]);
    }
}

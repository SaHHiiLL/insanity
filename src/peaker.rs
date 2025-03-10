#[derive(Debug, Clone, Copy)]
pub(crate) struct Cursor<'a, T> {
    items: &'a [T],
    idx: usize,
}

impl<'a, T> Cursor<'a, T> {
    pub(crate) fn new(items: &'a [T]) -> Self {
        Self { items, idx: 0 }
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
    }

    pub(crate) fn peak(&self) -> Option<&T> {
        self.items.get(self.idx + 1)
    }

    pub(crate) fn idx(&self) -> usize {
        self.idx
    }

    pub(crate) fn get(&self) -> Option<&T> {
        self.items.get(self.idx)
    }

    pub(crate) fn slice_x_y(&self, range: std::ops::Range<usize>) -> Option<&[T]> {
        // no need to check for X >= 0, type system already insures that
        if range.end < self.items.len() {
            Some(&self.items[range])
        } else {
            None
        }
    }

    pub(crate) fn slice_n(&self, n: usize) -> Option<&[T]> {
        if n >= self.items.len() {
            None
        } else {
            Some(&self.items[self.idx..=n])
        }
    }
}

impl std::fmt::Display for Cursor<'_, char> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.items.iter().collect::<String>())
    }
}

impl Cursor<'_, char> {
    pub(crate) fn slice_x_y_stirng(&self, range: std::ops::Range<usize>) -> Option<String> {
        // no need to check for X >= 0, type system already insures that
        if range.end < self.items.len() {
            Some(self.items[range].iter().collect())
        } else {
            None
        }
    }
}

pub trait MoveBackIterator {
    type Item;
    fn prev(&mut self) -> Option<Self::Item>;
}

impl<'a, T> MoveBackIterator for Cursor<'a, T> {
    type Item = &'a T;

    fn prev(&mut self) -> Option<Self::Item> {
        if self.idx == 0 {
            None
        } else {
            self.idx = self.idx.saturating_sub(1);
            let res = self.items.get(self.idx)?;
            Some(res)
        }
    }
}

impl<'a, T> Iterator for Cursor<'a, T> {
    type Item = &'a T;
    // Next will give the current value and advance the pointer
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.items.get(self.idx)?;
        self.idx = self.idx.saturating_add(1);
        Some(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new() {
        let items = [1, 2, 3];
        let cursor = Cursor::new(&items);
        assert_eq!(cursor.idx(), 0);
    }

    #[test]
    fn test_get() {
        let items = [1, 2, 3];
        let cursor = Cursor::new(&items);
        assert_eq!(cursor.get(), Some(&1));
    }

    #[test]
    fn test_peak() {
        let items = [1, 2, 3];
        let cursor = Cursor::new(&items);
        assert_eq!(cursor.peak(), Some(&2));
    }

    #[test]
    fn test_slice_x_y() {
        let items = [1, 2, 3, 4, 5];
        let cursor = Cursor::new(&items);
        assert_eq!(cursor.slice_x_y(1..4), Some(&[2, 3, 4][..]));
        assert_eq!(cursor.slice_x_y(4..6), None);
    }

    #[test]
    fn test_slice_n() {
        let items = [1, 2, 3, 4, 5];
        let cursor = Cursor::new(&items);
        assert_eq!(cursor.slice_n(2), Some(&[1, 2, 3][..]));
        assert_eq!(cursor.slice_n(5), None);
    }

    #[test]
    fn test_iterator() {
        let items = [1, 2, 3];
        let mut cursor = Cursor::new(&items);
        assert_eq!(cursor.next(), Some(&1));
        assert_eq!(cursor.next(), Some(&2));
        assert_eq!(cursor.next(), Some(&3));
        assert_eq!(cursor.next(), None);
    }

    #[test]
    fn test_empty_cursor() {
        let items: [i32; 0] = [];
        let mut cursor = Cursor::new(&items);
        assert_eq!(cursor.get(), None);
        assert_eq!(cursor.next(), None);
        assert_eq!(cursor.prev(), None);
        assert_eq!(cursor.slice_x_y(0..1), None);
        assert_eq!(cursor.slice_n(1), None);
    }

    #[test]
    fn test_cursor_with_large_list() {
        let items: Vec<i32> = (0..100).collect();
        let mut cursor = Cursor::new(&items);
        assert_eq!(cursor.get(), Some(&0));
        for i in 0..100 {
            assert_eq!(cursor.next(), Some(&items[i]));
        }
        assert_eq!(cursor.next(), None);
        assert_eq!(cursor.prev(), Some(&99));
        assert_eq!(cursor.prev(), Some(&98));
        assert_eq!(cursor.prev(), Some(&97));
    }

    #[test]
    fn test_cursor_boundaries() {
        let items = [10, 20, 30, 40];
        let mut cursor = Cursor::new(&items);
        assert_eq!(cursor.next(), Some(&10));
        assert_eq!(cursor.next(), Some(&20));
        assert_eq!(cursor.next(), Some(&30));
        assert_eq!(cursor.next(), Some(&40));
        assert_eq!(cursor.next(), None);
        assert_eq!(cursor.prev(), Some(&40));
        assert_eq!(cursor.prev(), Some(&30));
        assert_eq!(cursor.prev(), Some(&20));
        assert_eq!(cursor.prev(), Some(&10));
        dbg!(&cursor);
        assert_eq!(cursor.prev(), None);
    }
}

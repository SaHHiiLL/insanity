pub(crate) struct Peaker<'a, T> {
    items: &'a [T],
    idx: usize,
}

impl<'a, T> Peaker<'a, T> {
    pub(crate) fn new(items: &'a [T]) -> Self {
        Self { items, idx: 0 }
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

    pub(crate) fn slice_x_y(&self, x: usize, y: usize) -> Option<&[T]> {
        if x >= self.items.len() {
            return None;
        }
        if y >= self.items.len() {
            return None;
        }
        Some(&self.items[x..=y])
    }

    pub(crate) fn slice_n(&self, n: usize) -> Option<&[T]> {
        if n >= self.items.len() {
            None
        } else {
            Some(&self.items[self.idx..=n])
        }
    }
}

impl<'a, T> Iterator for Peaker<'a, T> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        let res = self.items.get(self.idx)?;
        self.idx += 1;
        Some(res)
    }
}

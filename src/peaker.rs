pub(crate) struct Peaker<'a, T> {
    items: &'a [T],
    curr: usize,
}

impl<'a, T> Peaker<'a, T> {
    pub(crate) fn new(items: &'a [T]) -> Self {
        Self { items, curr: 0 }
    }

    pub(crate) fn peak(&self) -> Option<&T> {
        self.items.get(self.curr + 1)
    }

    pub(crate) fn curr(&self) -> Option<&T> {
        self.items.get(self.curr)
    }
}

impl<'a, T> Iterator for Peaker<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.items.get(self.curr)?;
        self.curr += 1;
        Some(res)
    }
}

pub trait Stack<'a>
where
    Self: 'a,
{
    type Value;
    type Input;
    type Output;

    fn with_parent(parent: Option<&'a Self>) -> Self;

    fn parent(&self) -> Option<&'a Self>;
    fn values(&self) -> &Vec<Self::Value>;
    fn values_mut(&mut self) -> &mut Vec<Self::Value>;

    fn find_map(index: usize, value: &Self::Value, input: &Self::Input) -> Option<Self::Output>;

    fn push(&mut self, value: Self::Value) {
        self.values_mut().push(value);
    }
    fn pop(&mut self, index: usize) {
        self.values_mut().remove(index);
    }
    fn top_index(&self) -> usize {
        self.values().len() - 1
    }
    fn swap_top(&mut self, new: Self::Value) {
        *self.values_mut().last_mut().unwrap() = new;
    }
    fn pop_top(&mut self) -> Self::Value {
        self.values_mut().pop().unwrap()
    }
    fn pop_top_n(&mut self, n: usize) -> Vec<Self::Value> {
        let start = self.values().len() - n;
        self.values_mut().split_off(start)
    }

    fn find(&self, input: &Self::Input) -> Option<(Self::Output, u16)> {
        let mut cur = Some(self);
        let mut up_levels = 0;

        while let Some(stack) = cur {
            let Some(output) = stack
                .values()
                .iter()
                .enumerate()
                .rev()
                .find_map(|(index, value)| Self::find_map(index, value, input))
            else {
                cur = stack.parent();
                up_levels += 1;
                continue;
            };
            return Some((output, up_levels));
        }

        None
    }
}

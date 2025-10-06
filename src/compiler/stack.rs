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

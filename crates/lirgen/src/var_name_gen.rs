#[derive(Default, Debug)]
pub struct VariableNameGenerator(u64);

impl VariableNameGenerator {
    pub fn new() -> Self {
        Self(0)
    }
    pub fn generate(&mut self) -> u64 {
        let id = self.0;

        self.0 += 1;

        id
    }

    pub fn reset(&mut self) {
        self.0 = 0
    }
}

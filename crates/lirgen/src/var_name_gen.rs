#[derive(Default, Debug)]
pub struct VariableNameGenerator(u64);

impl VariableNameGenerator {
    pub fn new() -> Self {
        Self(0)
    }
    pub fn generate(&mut self) -> String {
        let id = self.0;

        self.0 += 1;

        id.to_string()
    }

    pub fn reset(&mut self) {
        self.0 = 0
    }
}

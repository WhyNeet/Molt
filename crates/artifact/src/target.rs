use inkwell::{
    targets::{CodeModel, RelocMode, Target, TargetMachine, TargetTriple},
    OptimizationLevel,
};

pub struct TargetInfo {
    pub triple: TargetTriple,
    pub cpu: String,
    pub features: String,
    pub name: String,
    pub description: String,
}

impl TargetInfo {
    pub fn new_native() -> Self {
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();

        Self {
            name: target.get_name().to_str().unwrap().to_string(),
            description: target.get_description().to_str().unwrap().to_string(),
            cpu: TargetMachine::get_host_cpu_name().to_string(),
            features: TargetMachine::get_host_cpu_features()
                .to_str()
                .unwrap()
                .to_string(),
            triple,
        }
    }
}

use std::path::Path;

use inkwell::{
    module::Module,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    OptimizationLevel,
};

#[derive(Debug)]
pub struct BuildArtifactGenerator {
    triple: TargetTriple,
    profile: BuildProfile,
}

impl Default for BuildArtifactGenerator {
    fn default() -> Self {
        Self {
            triple: TargetMachine::get_default_triple(),
            profile: BuildProfile::default(),
        }
    }
}

impl BuildArtifactGenerator {
    pub fn new(triple: TargetTriple) -> Self {
        Self {
            triple,
            profile: BuildProfile::default(),
        }
    }

    pub fn with_profile(triple: TargetTriple, profile: BuildProfile) -> Self {
        Self { triple, profile }
    }

    fn get_target(&self) -> Target {
        Target::initialize_native(&InitializationConfig::default()).unwrap();
        Target::from_triple(&self.triple).unwrap()
    }

    pub fn produce_object_file(&self, module: &Module<'_>, path: &Path) {
        let target = self.get_target();
        let machine = target
            .create_target_machine(
                &self.triple,
                "generic",
                "",
                self.profile.optimization,
                self.profile.reloc,
                self.profile.model,
            )
            .unwrap();

        machine
            .write_to_file(module, FileType::Object, path)
            .unwrap();
    }

    pub fn produce_asm_file(&self, module: &Module<'_>, path: &Path) {
        let target = self.get_target();
        let machine = target
            .create_target_machine(
                &self.triple,
                "generic",
                "",
                self.profile.optimization,
                self.profile.reloc,
                self.profile.model,
            )
            .unwrap();

        machine
            .write_to_file(module, FileType::Assembly, path)
            .unwrap();
    }

    pub fn produce_bitcode_file(&self, module: &Module<'_>, path: &Path) {
        module.write_bitcode_to_path(path);
    }
}

#[derive(Debug, Default)]
pub struct BuildProfile {
    pub optimization: OptimizationLevel,
    pub reloc: RelocMode,
    pub model: CodeModel,
}

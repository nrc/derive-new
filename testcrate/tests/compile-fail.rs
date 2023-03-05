extern crate compiletest_rs as compiletest;

use std::env;
use std::path::PathBuf;

fn run_mode(mode: &'static str) {
    let mut config = compiletest::Config {
        mode: mode.parse().expect("invalid mode"),
        ..Default::default()
    };
    let mut me = env::current_exe().unwrap();
    me.pop();
    config.target_rustcflags = Some(format!("-L {}", me.display()));
    let src = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    config.src_base = src.join("tests").join(mode);

    me.pop();
    me.pop();
    config.build_base = me.join("tests").join(mode);
    config.filter = env::args().nth(1);
    config.clean_rmeta();

    compiletest::run_tests(&config);
}

fn main() {
    run_mode("compile-fail");
}

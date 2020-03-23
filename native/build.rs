extern crate neon_build;

use std::process::Command;

fn main() {
    neon_build::setup(); // must be called in build.rs

    // add project-specific build logic here...
    Command::new("openssl").args(&["genrsa", "-out", "src/private_rsa_key.pem", "4096"])
        .status().unwrap();
    Command::new("openssl").args(&["rsa", "-in", "src/private_rsa_key.pem", "-outform", "DER", "-out", "src/private_rsa_key.der"])
        .status().unwrap();
    Command::new("openssl").args(&["rsa", "-in", "src/private_rsa_key.der", "-inform", "DER", "-RSAPublicKey_out", "-outform", "DER", "-out", "src/public_key.der"])
        .status().unwrap();
}

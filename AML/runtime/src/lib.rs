use std::io::{self, Write};

#[no_mangle]
pub extern "C" fn print_int(n: i64) {
    print!("{}", n);
    io::stdout().flush().unwrap()
}

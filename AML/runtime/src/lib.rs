use std::io::{self, Write};
use std::mem::transmute;
use std::process::abort;

#[repr(C)]
pub struct Closure {
    code: *const (),
    arity: i64,
    applied: i64,
    args: Vec<i64>,
}

macro_rules! call_with_i64_args {
    ($code:expr, $args:expr, $argc:expr) => {{
        let res: i64;
        match $argc {
            0 => {
                let f: extern "C" fn() -> i64 = transmute($code);
                res = f();
            }
            1 => {
                let f: extern "C" fn(i64) -> i64 = transmute($code);
                res = f($args[0]);
            }
            2 => {
                let f: extern "C" fn(i64, i64) -> i64 = transmute($code);
                res = f($args[0], $args[1]);
            }
            3 => {
                let f: extern "C" fn(i64, i64, i64) -> i64 = transmute($code);
                res = f($args[0], $args[1], $args[2]);
            }
            4 => {
                let f: extern "C" fn(i64, i64, i64, i64) -> i64 = transmute($code);
                res = f($args[0], $args[1], $args[2], $args[3]);
            }
            5 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64) -> i64 = transmute($code);
                res = f($args[0], $args[1], $args[2], $args[3], $args[4]);
            }
            6 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 = transmute($code);
                res = f($args[0], $args[1], $args[2], $args[3], $args[4], $args[5]);
            }
            7 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 = transmute($code);
                res = f(
                    $args[0], $args[1], $args[2], $args[3], $args[4], $args[5], $args[6],
                );
            }
            8 => {
                let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 =
                    transmute($code);
                res = f(
                    $args[0], $args[1], $args[2], $args[3], $args[4], $args[5], $args[6], $args[7],
                );
            }
            _ => {
                // TODO: add support of partial application of functions with more than 8 args
                eprintln!("fatal: unsupported arity > 8");
                abort();
            }
        }
        res
    }};
}

#[no_mangle]
pub extern "C" fn print_int(n: i64) {
    print!("{}", n);
    io::stdout().flush().unwrap();
}

#[no_mangle]
pub extern "C" fn closure_alloc(func: *const (), arity: i64) -> i64 {
    let clos = Closure {
        code: func,
        arity,
        applied: 0,
        args: Vec::with_capacity(arity as usize),
    };
    let ptr = Box::into_raw(Box::new(clos));
    ptr as i64
}

#[no_mangle]
/// Calls a closure with the given raw pointer and arguments.
///
/// # Safety
///
/// This function dereferences raw pointers (`clos_raw`, `argv`),
/// so the caller must ensure that:
/// - `clos_raw` is a valid pointer to a `Closure` object.
/// - `argv` points to a contiguous array of at least `argc` valid `i64` values.
/// - The closure code must not free or mutate `argv` while it is being read.
///
/// Violating these conditions may cause undefined behavior.
pub unsafe extern "C" fn closure_apply(clos_raw: i64, argc: i64, argv: *const i64) -> i64 {
    unsafe {
        let clos_ptr = clos_raw as *mut Closure;
        if clos_ptr.is_null() {
            eprintln!("fatal: closure_apply on null pointer");
            abort();
        }

        let src = &*clos_ptr;
        let total = src.arity as usize;
        let have = src.applied as usize;

        let mut args = Vec::with_capacity(total);
        args.extend_from_slice(&src.args);
        for i in 0..(argc as usize) {
            let val = *argv.add(i);
            args.push(val);
        }

        let got = have + argc as usize;

        if got < total {
            // partial application
            let new_clos = Closure {
                code: src.code,
                arity: src.arity,
                applied: got as i64,
                args,
            };
            Box::into_raw(Box::new(new_clos)) as i64
        } else if got == total {
            // full application
            call_with_i64_args!(src.code, args, total)
        } else {
            eprintln!("fatal: over-application (arity={}, got={})", total, got);
            abort();
        }
    }
}

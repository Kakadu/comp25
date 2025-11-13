use std::io::{self, Write};
use std::process::abort;
use std::ptr::addr_of_mut;

const HEAP_WORDS: usize = 128 * 1024;
const HEAP_SIZE_BYTES: usize = HEAP_WORDS * 8;

// old space
static mut HEAP_1: [i64; HEAP_WORDS] = [0; HEAP_WORDS];
// new space
static mut HEAP_2: [i64; HEAP_WORDS] = [0; HEAP_WORDS];

static mut FIRST_HEAP_START: *mut i64 = std::ptr::null_mut();

static mut OLD_SPACE_START: *mut i64 = std::ptr::null_mut();
static mut OLD_SPACE_END: *mut i64 = std::ptr::null_mut();
static mut ALLOC_PTR: *mut i64 = std::ptr::null_mut();

static mut NEW_SPACE_START: *mut i64 = std::ptr::null_mut();
static mut NEW_SPACE_END: *mut i64 = std::ptr::null_mut();

static mut GC_COUNT: i64 = 0;
static mut TOTAL_ALLOCATED: i64 = 0;

// should be set by codegen
#[no_mangle]
pub static mut ML_STACK_BASE: *const i64 = std::ptr::null();

// object layout
// heap objects are blocks of i64
// word 0: header (size, tag)
// word 1..n: obj data
// header format: (size << 10) | tag. tag is 8 bits, size is length in words
const CLOSURE_TAG: i64 = 247;
const FORWARDING_TAG: i64 = 255; // forwarding pointer to mark moved object

// closure layout:
// word 0: header
// word 1: code pointer
// word 2: arity (untagged)
// word 3: applied (untagged)
// word 4..n: applied args (tagged)
const CLOSURE_CODE_PTR_OFFSET: usize = 1;
const CLOSURE_ARITY_OFFSET: usize = 2;
const CLOSURE_APPLIED_OFFSET: usize = 3;
const CLOSURE_ARGS_OFFSET: usize = 4;

// === helpers ===
fn make_header(size_in_words: i64, tag: i64) -> i64 {
    (size_in_words << 10) | tag
}

fn get_size_from_header(header: i64) -> usize {
    (header >> 10) as usize
}

fn get_tag_from_header(header: i64) -> i64 {
    header & 0xFF
}

/// copy object from old-space to new-space
unsafe fn copy_object(obj_ptr: *mut i64, allocptr: &mut *mut i64) -> *mut i64 {
    // check if the object has already been forwarded
    let header = *obj_ptr;
    if get_tag_from_header(header) == FORWARDING_TAG {
        // update the root with the address of the copied object by forwarding pointer
        let new_ptr_i64 = *(obj_ptr.add(1));
        return new_ptr_i64 as *mut i64;
    }

    let size_in_words = get_size_from_header(header);
    let new_obj_ptr = *allocptr;
    *allocptr = allocptr.add(size_in_words);

    std::ptr::copy_nonoverlapping(obj_ptr, new_obj_ptr, size_in_words);

    // set forwarding pointer in old object
    *obj_ptr.add(0) = make_header(0, FORWARDING_TAG);
    *(obj_ptr.add(1)) = new_obj_ptr as i64;

    new_obj_ptr
}

/// Initializes the garbage collector's heap spaces.
///
/// # Safety
///
/// This function modifies global variables.
/// - It must be called once at the beginning of the program and before any other heap operations.
#[no_mangle]
pub unsafe extern "C" fn heap_init() {
    OLD_SPACE_START = addr_of_mut!(HEAP_1[0]);
    OLD_SPACE_END = OLD_SPACE_START.add(HEAP_WORDS);
    ALLOC_PTR = OLD_SPACE_START;

    NEW_SPACE_START = addr_of_mut!(HEAP_2[0]);
    NEW_SPACE_END = NEW_SPACE_START.add(HEAP_WORDS);

    FIRST_HEAP_START = OLD_SPACE_START;
}

/// Gets the raw pointer to the start of the current heap space (old-space).
///
/// # Safety
///
/// - `heap_init` must be called before this function.
/// - The returned pointer is only valid until the next garbage collection.
#[no_mangle]
pub unsafe extern "C" fn get_heap_start(_argc: i64, _argv: *const i64) -> i64 {
    OLD_SPACE_START as i64
}

/// Gets the raw pointer to the current top of the heap, where the next object will be allocated.
///
/// # Safety
///
/// - `heap_init` must be called before this function.
/// - The returned pointer is only valid until the next allocation or garbage collection.
#[no_mangle]
pub unsafe extern "C" fn get_heap_fin(_argc: i64, _argv: *const i64) -> i64 {
    ALLOC_PTR as i64
}

/// Prints debug info of the current heap and GC state to stdout.
///
/// # Safety
///
/// - `heap_init` must be called before this function.
#[no_mangle]
pub unsafe extern "C" fn print_gc_status(_argc: i64, _argv: *const i64) -> i64 {
    let old_start = OLD_SPACE_START;
    let old_end = OLD_SPACE_END;
    let alloc_ptr = ALLOC_PTR;
    let new_start = NEW_SPACE_START;
    let new_end = NEW_SPACE_END;
    let gc_count = GC_COUNT;
    let total_allocated = TOTAL_ALLOCATED;

    let is_in_first_bank = old_start == FIRST_HEAP_START;

    const VIRTUAL_BASE: usize = 0x0;

    let virtual_bank_base = if is_in_first_bank {
        VIRTUAL_BASE
    } else {
        VIRTUAL_BASE + HEAP_SIZE_BYTES
    };

    let physical_alloc_offset = alloc_ptr as usize - old_start as usize;
    let physical_old_end_offset = old_end as usize - old_start as usize;

    let physical_new_start_offset = new_start as isize - old_start as isize;
    let physical_new_end_offset = new_end as isize - old_start as isize;

    let virt_old_start = virtual_bank_base;
    let virt_alloc_ptr = virtual_bank_base + physical_alloc_offset;
    let virt_old_end = virtual_bank_base + physical_old_end_offset;
    let virt_new_start = (virtual_bank_base as isize + physical_new_start_offset) as usize;
    let virt_new_end = (virtual_bank_base as isize + physical_new_end_offset) as usize;

    let heap_size = physical_old_end_offset;
    let used = physical_alloc_offset;

    println!(" \n=== GC STATUS ===");
    println!("old space start:  {:#x}", virt_old_start);
    println!("old space end:    {:#x}", virt_old_end);
    println!("alloc pointer:    {:#x}", virt_alloc_ptr);
    println!("new space start:  {:#x}", virt_new_start);
    println!("new space end:    {:#x}", virt_new_end);
    println!("heap size: {} bytes", heap_size);
    println!("used (old space): {} bytes", used);
    println!("collects count: {}", gc_count);
    println!("allocations in total: {} bytes", total_allocated);
    println!("=================");
    1
}

unsafe fn allocate(size_in_words: i64) -> *mut i64 {
    let new_alloc_ptr = ALLOC_PTR.add(size_in_words as usize);

    if new_alloc_ptr > OLD_SPACE_END {
        collect();

        let new_alloc_ptr_after_gc = ALLOC_PTR.add(size_in_words as usize);
        if new_alloc_ptr_after_gc > OLD_SPACE_END {
            eprintln!("fatal: out of memory after GC");
            abort();
        }

        let result_ptr = ALLOC_PTR;
        ALLOC_PTR = new_alloc_ptr_after_gc;
        TOTAL_ALLOCATED += size_in_words * 8;
        result_ptr
    } else {
        // update ALLOC_PTR if there is enough space
        let result_ptr = ALLOC_PTR;
        ALLOC_PTR = new_alloc_ptr;
        TOTAL_ALLOCATED += size_in_words * 8;
        result_ptr
    }
}

/// Performs copying garbage collection algorithm.
/// https://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm
/// https://en.wikipedia.org/wiki/Cheney%27s_algorithm
///
/// # Safety
///
/// This function requires on environment set up by code generator.
/// - `heap_init` must be called before this function.
/// - The `ML_STACK_BASE` variable must be set to a pointer to the bottom of the stack frame before this function is called.
/// - The stack region between the current `sp` and `ML_STACK_BASE` must only contain valid runtime values: tagged integers or valid 64-bit pointers into the `OLD_SPACE`. Any other bit pattern will cause undefined behavior.
#[no_mangle]
pub unsafe extern "C" fn collect() {
    let mut allocptr = NEW_SPACE_START;
    let mut scanptr = NEW_SPACE_START;

    // find and copy roots
    let sp_top: *const i64;
    std::arch::asm!("mv {}, sp", out(reg) sp_top);
    let stack_bottom = ML_STACK_BASE;

    let mut current_stack_ptr = sp_top as *mut i64;

    while current_stack_ptr < stack_bottom as *mut i64 {
        let value = *current_stack_ptr;

        if (value & 1) == 0
            && (value as *mut i64) >= OLD_SPACE_START
            && (value as *mut i64) < ALLOC_PTR
        {
            let new_ptr = copy_object(value as *mut i64, &mut allocptr);
            *current_stack_ptr = new_ptr as i64;
        }
        current_stack_ptr = current_stack_ptr.add(1);
    }

    while scanptr < allocptr {
        let obj_to_scan = scanptr;
        let header = *obj_to_scan.add(0);
        let tag = get_tag_from_header(header);

        if tag == CLOSURE_TAG {
            let applied_count = *obj_to_scan.add(CLOSURE_APPLIED_OFFSET);
            let args_ptr = obj_to_scan.add(CLOSURE_ARGS_OFFSET);

            // find pointers and forward them
            for i in 0..applied_count {
                let field_ptr = args_ptr.add(i as usize);
                let value = *field_ptr;

                if (value & 1) == 0
                    && (value as *mut i64) >= OLD_SPACE_START
                    && (value as *mut i64) < ALLOC_PTR
                {
                    let new_ptr = copy_object(value as *mut i64, &mut allocptr);
                    *field_ptr = new_ptr as i64;
                }
            }
        }
        // TODO(tuples): else if tag == TUPLE_TAG { TODO }

        let size_in_words = get_size_from_header(header);
        scanptr = scanptr.add(size_in_words);
    }

    // flip spaces
    let old_heap_start = OLD_SPACE_START;
    let old_heap_end = OLD_SPACE_END;

    OLD_SPACE_START = NEW_SPACE_START;
    OLD_SPACE_END = NEW_SPACE_END;
    ALLOC_PTR = allocptr;

    NEW_SPACE_START = old_heap_start;
    NEW_SPACE_END = old_heap_end;

    GC_COUNT += 1;
}

/// helper function to call a function with custom calling convention
#[inline(always)]
fn call_with_i64_args(code: *const (), args: &[i64]) -> i64 {
    unsafe {
        let f: extern "C" fn(i64, *const i64) -> i64 = std::mem::transmute(code);
        f(args.len() as i64, args.as_ptr())
    }
}

/// Prints a single integer to stdout.
/// This function expects a tagged integer and will untag it before printing.
///
/// # Safety
///
/// Caller must ensure:
/// - `argv` points to a valid memory region containing at least `argc` elements of type `i64`.
/// - `argc` must be exactly 1; otherwise the function will abort.
/// - The value at `argv[0]` must be a valid tagged integer.
#[no_mangle]
pub unsafe extern "C" fn print_int(argc: i64, argv: *const i64) -> i64 {
    if argc != 1 {
        eprintln!("fatal: print_int expects 1 arg, got {}", argc);
        abort();
    }
    let t_n = unsafe { *argv.add(0) };
    let n = t_n >> 1;
    print!("{}", n);
    io::stdout().flush().unwrap();
    t_n
}

/// Allocates a new closure object on the heap.
///
/// # Safety
///
/// The closure is initialized with 0 applied arguments. This function may trigger a garbage collection if the heap is full.
/// - `heap_init` must be called before this function.
/// - The `func` pointer must be a valid, non-null pointer to function code.
/// - This function calls `allocate`, which may trigger `collect`. Therefore, all safety invariants required by `collect` must be handled before calling this function.
#[no_mangle]
pub unsafe extern "C" fn closure_alloc(func: *const (), arity: i64) -> i64 {
    let size_in_words: i64 = 4;
    let new_closure_ptr = allocate(size_in_words);

    *new_closure_ptr.add(0) = make_header(size_in_words, CLOSURE_TAG);
    *(new_closure_ptr.add(CLOSURE_CODE_PTR_OFFSET) as *mut *const ()) = func;
    *new_closure_ptr.add(CLOSURE_ARITY_OFFSET) = arity;
    *new_closure_ptr.add(CLOSURE_APPLIED_OFFSET) = 0;

    new_closure_ptr as i64
}

/// Applies new arguments to a closure. Performs partial application if argument count is insufficient,
/// and invokes the underlying function when enough arguments are provided.
/// The original closure is leaked intentionally (the caller is responsible for memory management).
///
/// # Safety
///
/// Caller must ensure:
/// - `clos_raw` is a valid pointer (non-null) to a previously allocated `Closure` object created by [`closure_alloc`].
/// - `argv` points to a contiguous memory region containing at least `argc` `i64` values.
/// - `argc` must be non-negative.
#[no_mangle]
pub unsafe extern "C" fn closure_apply(clos_raw: i64, argc: i64, argv: *const i64) -> i64 {
    let clos_ptr = clos_raw as *mut i64;

    let code_ptr = *(clos_ptr.add(CLOSURE_CODE_PTR_OFFSET) as *mut *const ());
    let total_arity = *clos_ptr.add(CLOSURE_ARITY_OFFSET);
    let old_applied_count = *clos_ptr.add(CLOSURE_APPLIED_OFFSET);
    let old_args_ptr = clos_ptr.add(CLOSURE_ARGS_OFFSET);

    let new_applied_count = old_applied_count + argc;

    if new_applied_count < total_arity {
        let new_size_in_words = 4 + new_applied_count;
        let new_closure_ptr = allocate(new_size_in_words);

        *new_closure_ptr.add(0) = make_header(new_size_in_words, CLOSURE_TAG);
        *(new_closure_ptr.add(CLOSURE_CODE_PTR_OFFSET) as *mut *const ()) = code_ptr;
        *new_closure_ptr.add(CLOSURE_ARITY_OFFSET) = total_arity;
        *new_closure_ptr.add(CLOSURE_APPLIED_OFFSET) = new_applied_count;

        let new_args_ptr = new_closure_ptr.add(CLOSURE_ARGS_OFFSET);

        std::ptr::copy_nonoverlapping(old_args_ptr, new_args_ptr, old_applied_count as usize);
        std::ptr::copy_nonoverlapping(
            argv,
            new_args_ptr.add(old_applied_count as usize),
            argc as usize,
        );

        new_closure_ptr as i64
    } else if new_applied_count == total_arity {
        let mut all_args: Vec<i64> = Vec::with_capacity(total_arity as usize);
        all_args.extend_from_slice(std::slice::from_raw_parts(
            old_args_ptr,
            old_applied_count as usize,
        ));
        all_args.extend_from_slice(std::slice::from_raw_parts(argv, argc as usize));

        call_with_i64_args(code_ptr, &all_args)
    } else {
        let mut full_call_args: Vec<i64> = Vec::with_capacity(total_arity as usize);
        full_call_args.extend_from_slice(std::slice::from_raw_parts(
            old_args_ptr,
            old_applied_count as usize,
        ));

        let args_needed = total_arity - old_applied_count;
        full_call_args.extend_from_slice(std::slice::from_raw_parts(argv, args_needed as usize));

        let result_closure_raw = call_with_i64_args(code_ptr, &full_call_args);

        let rest_argc = new_applied_count - total_arity;
        let rest_argv = argv.add(args_needed as usize);

        closure_apply(result_closure_raw, rest_argc, rest_argv)
    }
}

#include "call-runtime.h"
#include <stdio.h>
#include <stdlib.h>
#include <ffi.h>

int64_t call_function(void *func, int64_t arity, int64_t *args) {
    ffi_cif cif;

    ffi_type *args_t[arity];
    void *args_v[arity];
    for (int64_t i = 0; i < arity; i++) {
        args_t[i] = &ffi_type_sint64;
        args_v[i] = &args[i];
    }

    if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arity, &ffi_type_sint64, args_t) != FFI_OK) {
        fprintf(stderr, "closure call failed");
        exit(1);
    }

    ffi_sarg ret;
    ffi_call(&cif, FFI_FN(func), &ret, args_v);

    return ret;
}


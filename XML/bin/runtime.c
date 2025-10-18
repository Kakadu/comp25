/* Copyright 2023-2024, Kakadu and contributors */
/* SPDX-License-Identifier: LGPL-3.0-or-later */

#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>

void print_int(int64_t n) {
    printf("%" PRId64, n);
    fflush(stdout);
}
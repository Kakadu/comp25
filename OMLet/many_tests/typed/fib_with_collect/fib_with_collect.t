Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh fib_with_collect.ml || true
  8
  $ ../build_and_run.sh fib_with_collect.ml --gc || true
  [GC] Out of memory in omlet gc
  $ ../build_and_run.sh fib_with_collect.ml --gc 10000 || true
  8

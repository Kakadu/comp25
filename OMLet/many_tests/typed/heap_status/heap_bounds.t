Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh heap_bounds.ml || true
  1000
  $ ../build_and_run.sh heap_bounds.ml --gc 10000 || true
  10000

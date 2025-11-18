Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh large_print.ml || true
  420
  $ ../build_and_run.sh large_print.ml --gc || true
  420
  $ ../build_and_run.sh large_print.ml --gc 10000 || true
  420

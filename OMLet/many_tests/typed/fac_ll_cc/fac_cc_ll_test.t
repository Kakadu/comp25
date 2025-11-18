Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh ../../manytests/typed/012faccps.ml || true
  720
  $ ../build_and_run.sh ../../manytests/typed/012faccps.ml --gc || true
  [GC] Out of memory in omlet gc
  $ ../build_and_run.sh ../../manytests/typed/012faccps.ml --gc 10000 || true
  720

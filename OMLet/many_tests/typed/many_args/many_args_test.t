Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh ../../manytests/typed/004manyargs.ml  || true
  1111111111110100
  $ ../build_and_run.sh ../../manytests/typed/004manyargs.ml --gc || true
  1111111111110100
  $ ../build_and_run.sh ../../manytests/typed/004manyargs.ml --gc 10000 || true
  1111111111110100

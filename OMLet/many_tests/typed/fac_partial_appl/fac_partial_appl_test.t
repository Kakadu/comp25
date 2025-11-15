Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh ../../manytests/typed/010faccps_ll.ml || true
  24
  $ ../build_and_run.sh ../../manytests/typed/010faccps_ll.ml --gc || true
  24
  $ ../build_and_run.sh ../../manytests/typed/010faccps_ll.ml --gc 10000 || true
  24

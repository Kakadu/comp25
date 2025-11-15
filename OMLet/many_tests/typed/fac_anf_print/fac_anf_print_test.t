Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../build_and_run.sh fac_anf_print.ml || true
  24
  $ ../build_and_run.sh fac_anf_print.ml --gc || true
  24
  $ ../build_and_run.sh fac_anf_print.ml --gc 10000 || true
  24

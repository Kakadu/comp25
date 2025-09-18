(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Qchecker

let () =
  let _ : int = run_gen ~show_passed:false ~show_shrinker:false ~count:10 in
  ()
;;

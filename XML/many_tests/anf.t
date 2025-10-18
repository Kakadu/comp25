  $ dune exec ./../bin/XML.exe -- --anf <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)
  let rec fac =
  let t_5 =
    fun n ->
      let t_0 =
        (n = 0)
        in
        let t_4 =
          if t_0 then1else
            let t_1 = (n - 1) inlet t_2 = fac (t_1) inlet t_3 = (n * t_2) int_3
          in
          t_4
    in
    t_5;;let main =
  let t_6 = fac (4) inlet t_7 = print_int (t_6) int_7;;

  $ ../bin/XML.exe --anf <<EOF
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)
  let rec fib =
  let t_7 =
    fun n ->
      let t_0 =
        (n < 2)
        in
        let t_6 =
          if t_0 thennelse
            let t_1 =
              (n - 1)
              in
              let t_2 =
                fib (t_1)
                in
                let t_3 =
                  (n - 2)
                  in
                  let t_4 = fib (t_3) inlet t_5 = (t_2 + t_4) int_5
          in
          t_6
    in
    t_7;;let main =
  let t_8 = fib (6) inlet t_9 = print_int (t_8) int_9;;

  $ ../bin/XML.exe --anf <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 = 1 else 1 = 1)
  >           then 0 else 1 in
  >   large x
  let large =
  let t_4 =
    fun x ->
      let t_0 =
        (0 <> x)
        in
        let t_3 =
          if t_0 thenlet t_1 = print_int (0) int_1else
            let t_2 = print_int (1) int_2
          in
          t_3
    in
    t_4;;let main =
  let t_5 =
    (0 = 1)
    in
    let t_8 =
      if t_5 thenlet t_6 = (0 = 1) int_6else
        let t42 = print_int (42) inlet t_7 = (1 = 1) int_7
      in
      let t_11 =
        if t_8 thenlet t_9 = (0 = 1) int_9elselet t_10 = (1 = 1) int_10
        in
        let x = if t_11 then0else1 inlet t_12 = large (x) int_12;;

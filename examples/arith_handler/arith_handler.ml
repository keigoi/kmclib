open Kmclib

let rec arith acc : [%kmc.check.handler g.s] =
  `c
    (object
       method add x = `c (`result (x + acc, arith (x + acc)))

       method div x =
         if x = 0 then
           `c (`err ("div by zero", arith acc))
         else
           `c (`result (acc / x, arith (acc / x)))

       method stop () = ()
    end)

let rec cli (xs : int list) n : [%kmc.check.handler g.c] =
  match xs with
  | x :: xs ->
    `s
      (`add
        ( x
        , `s
            (object
               method result (_ : int) = cli xs n
            end) ))
  | [] ->
    `s
      (`div
        ( n
        , `s
            (object
               method result r =
                 print_endline ("average: " ^ string_of_int r);
                 `s (`stop ((), ()))

               method err msg =
                 print_endline msg;
                 `s (`stop ((), ()))
            end) ))

let () =
  let (KMC (s_runner, c_runner)) = [%kmc.gen.runner g (s, c)] in
  let t = Thread.create s_runner (arith 0) in
  c_runner (cli [ 10; 20; 30 ] 3);
  Thread.join t

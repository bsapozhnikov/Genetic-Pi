type monop = Sqrt;;

type binop = Div | Add;;

type exp =
  | Num of int
  | Monop of monop * int
  | Binop of binop * exp * exp;;

let string_of_binop (op : binop) =
  match op with
  | Div -> "/"
  | Add -> "+";;

let rec string_of_exp = fun (e : exp) : string ->
  match e with
  | Num(n) -> string_of_int n
  | Monop(Sqrt, n) -> "√" ^ (string_of_int n)
  | Binop(op, e1, e2) ->
     let s1 = string_of_exp e1 in
     let s2 = string_of_exp e2 in
     let o = string_of_binop op in
     "(" ^ s1 ^ o ^ s2 ^ ")";;

let print_exp = fun (e : exp) -> print_string (string_of_exp e);;

let func_of_binop (op : binop) =
  match op with
  | Div -> (/.)
  | Add -> (+.);;

let rec eval = fun (e : exp) : float ->
  match e with
  | Num(n) -> float_of_int n
  | Monop(Sqrt, n) -> Num n |> eval |> sqrt
  | Binop(op, e1, e2) ->
     let x1 = eval e1 in
     let x2 = eval e2 in
     let f = func_of_binop op in
     f x1 x2;;

let rec num_digits = fun (e : exp) : int ->
  match e with
  | Num(n) when abs n < 10 -> 1
  | Num(n) -> 1 + (num_digits (Num(n / 10)))
  | Monop(Sqrt, n) -> num_digits (Num(n))
  | Binop(_, e1, e2) -> (num_digits e1) + (num_digits e2)

let loss = fun (e : exp) : float ->
  (* let digits = e |> num_digits |> float_of_int in *)
  let approx = eval e in
  let actual = 3.14159265 in
  (* print_string "    loss ";
   * print_exp e;
   * print_string " => ";
   * print_float (abs_float (approx -. actual));
   * print_newline(); *)
  (* abs_float (approx -. actual) *. digits;; *)
  abs_float (approx -. actual);;

let vary_num = fun (n : int) : int ->
  let r = Random.float 1. in
  if r < 0.3 then n / 2
  else if r < 0.6 then n * 2
  else let r2 = Random.int 500 in n + r2 - 250;;

let rec vary_coeff = fun (e : exp) : exp ->
  (* print_string "vary_coeff\n  starting with ";
   * print_exp e;
   * print_newline (); *)
  let r = Random.float 1. in
  match e with
  | Num(n) -> Num(vary_num n)
  | Monop(op, n) -> Monop(op, (vary_num n))
  | Binop(op, e1, e2) ->
     if r < 0.5
     then ((*print_endline "  varying first sub-expression";*) Binop(op, vary_coeff e1, e2))
     else ((*print_endline "  varying second sub-expression";*) Binop(op, e1, vary_coeff e2))

let better = fun (e1 : exp) (e2 : exp) : exp ->
  let loss1 = loss e1 in
  let loss2 = loss e2 in
  if loss1 <= loss2 then e1 else e2

let rec tune_coeffs = fun (e : exp) (fuel : int) : exp ->
  print_string "tune_coeffs\n  starting with ";
  print_exp e;
  print_newline ();
  if fuel = 0 then (print_endline "  out of fuel; returning input"; e) else
    (let new_exp = vary_coeff e in
    print_string "  varied input to "; print_exp new_exp; print_newline ();
    tune_coeffs (better e new_exp) (fuel - 1));;

let rec complicate = fun (e : exp) : exp list ->
  match e with
  | Num(n) -> [e; Monop(Sqrt, n); Binop(Div, e, Num(1)); Binop(Add, e, Num(0))]
  | Monop(Sqrt, n) -> [e; Binop(Div, e, Num(1)); Binop(Add, e, Num(0))]
  | Binop(op, e1, e2) ->
     let e1's = complicate e1 in
     let e2's = complicate e2 in
     let e's1 = List.map (fun e1' -> Binop(op, e1', e2)) e1's in
     let e's2 = List.map (fun e2' -> Binop(op, e1, e2')) e2's in
     [e] @ e's1 @ e's2;;

let rec first_n : int -> exp list -> exp list =
  let rec first_n' = fun res n es ->
    if n = 0 then res else
      match es with
      | [] -> res
      | h::tl -> first_n' (h::res) (n - 1) tl in
  first_n' [];;

let random_choice = fun (es : exp list) : exp ->
  let choice = Random.int (List.length es) in
  List.nth es choice;;

let rec safe_int_of_float = fun (f : float) : int ->
  (* print_string "    ";
   * print_float f;
   * print_newline(); *)
  let inf = 1./.0. in
  if f = inf then max_int else int_of_float f;;

let rec prune = fun (n : int) (es : exp list) : exp list ->
  (* print_string "pruning\n  starting with ";
   * List.iter (fun e -> print_exp e; print_string ",") es;
   * print_newline (); *)
  let sorted = List.sort_uniq (fun e1 e2 -> (* print_string "  comparing "; print_exp e1; print_string " to "; print_exp e2; print_newline(); *) safe_int_of_float (100. *. ((loss e1) -. (loss e2)))) es in
  (* print_string "  sorted: ";
   * List.iter (fun e -> print_exp e; print_string ",") sorted;
   * print_newline (); *)
  first_n n sorted;;

let mutate = fun (e : exp) : exp ->
  let r = Random.float 1. in
  if r < 0.8 then vary_coeff e
  else let e's = complicate e in random_choice e's;;

(*
let rec evolve = fun (es : exp list) (fuel : int) : exp ->
  print_endline "evolving";
  List.iter (fun e -> print_string "  starting with "; print_exp e; print_newline ()) es;
  let e's : exp list = List.map (fun e -> tune_coeffs e (min fuel 50)) es in
  List.iter (fun e -> print_string "  tuned to "; print_exp e; print_newline ()) e's;
  let best_e = List.fold_left better (Num(1)) e's in
  print_string "  best one is "; print_exp best_e; print_newline ();
  if fuel < 50
  then (print_endline "  low fuel; returning best"; best_e)
  else (print_endline "  mutating"; evolve (complicate best_e) (fuel - 50));;
 *)

let rec evolve = fun (es : exp list) (fuel : int) : exp ->
  (* print_string "evolving\n  starting with ";
   * List.iter (fun e -> print_exp e; print_string ",") es;
   * print_newline (); *)
  let min_size = 25 in
  let max_size = 500 in
  if fuel = 0 then List.hd (prune 1 es)
  else if List.length es > max_size then evolve (prune min_size es) (fuel)
  else let mutant = mutate (random_choice es) in evolve (mutant::es) (fuel - 1);;

let print_func_call = fun (name : string) (f : exp -> exp) (e_in : exp) ->
  let e_out = f e_in in
  print_string (name ^ " ");
  print_exp e_in;
  print_string " => ";
  print_exp e_out;
  print_newline ();
  e_out;;

let rec simplify = fun (e : exp) : exp ->
  let rec simplify' e = match e with
    | Num(n) | Monop(Sqrt, n) -> e
    | Binop(Div, e', Num(0)) -> Num(-1)
    | Binop(Div, Num(n), Num(d)) when n mod d = 0 -> Num(n / d)
    | Binop(Add, Num(n1), Num(n2)) -> Num(n1 + n2)
    | Binop(op, e1, e2) ->
       let e1' = simplify e1 in
       let e2' = simplify e2 in
       Binop(op, e1', e2') in
  let e' = simplify' e in
  if e = e' then e else simplify e';;

(* let simplify = print_func_call "simplify" simplify;; *)

Random.self_init ();;

let rec optimize = fun (epoch_length : int) (num_epochs : int) ->
  if num_epochs = 0 then () else
    let e = Num(1) in
    let e = evolve [e] epoch_length in
    let e = simplify e in
    let x = eval e in
    let lss = loss e in
    if lss < 0.0000001
    then
      (print_exp e;
       print_string " => ";
       print_float x;
       print_string " [loss: ";
       print_float (loss e);
       print_endline "]")
    else optimize epoch_length (num_epochs - 1);;

optimize 6000 1000;;

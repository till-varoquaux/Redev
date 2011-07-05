(*

  Bzrozowski derivative's base re matcher.

  Taken from Norman Ramsey and Chrisian lindig's qc--
  http://www.cminusminus.org/rsync/qc--/cllib/rx.nw
  Inspired by a post by mark hopkins on comp.compilers
*)
(*
val zero : 'a rx                                (* never matches       *)
val unit : 'a rx                                (* matches empty input *)
val sym : 'a -> 'a rx                           (* 'x'                 *)
val many : 'a rx -> 'a rx                       (* e*                  *)
val some : 'a rx -> 'a rx                       (* e+                  *)
val opt : 'a rx -> 'a rx                        (* e?                  *)
val seq : 'a rx -> 'a rx -> 'a rx               (* e1 e2               *)
val alt : 'a rx -> 'a rx -> 'a rx               (* e1 | e2             *)
val ( ||| ) : 'a rx -> 'a rx -> 'a rx           (* e1 | e2             *)
val ( *** ) : 'a rx -> 'a rx -> 'a rx           (* e1 e2               *)

val matches : 'a rx -> 'a list -> bool
*)

type rx      =
  | Zero                                (* {}           *)
  | Unit                                (* ""           *)
  | Sym         of char                 (* 'x'          *)
  | Many        of rx                   (* e*           *)
  | Some        of rx                   (* e+           *)
  | Opt         of rx                   (* e?           *)
  | Seq         of rx * rx              (* e1 e2        *)
  | Alt         of rx * rx              (* e1 | e2      *)

(** *)
let rec to_string priority = function
  | Zero          -> "<false>"
  | Unit          -> "<true>"
  | Sym c         -> String.make 1 c
  | Many re       -> one priority 3 re "*"
  | Some re       -> one priority 3 re "+"
  | Opt re        -> one priority 3 re "?"
  | Seq (re1,re2) -> two priority 2 re1 re2 ""
  | Alt (re1,re2) -> two priority 1 re1 re2 "|"
and one parent_priority priority re op =
  if priority < parent_priority then
    "(" ^ to_string priority re ^ op ^ ")"
  else
    to_string priority re ^ op
and two parent_priority priority rel rer op =
  if priority < parent_priority then
    "(" ^ to_string priority rel ^ op ^ to_string priority rer ^ ")"
  else
    to_string priority rel ^ op ^ to_string priority rer

let to_string e = to_string 0 e

(** Smart constructor *)
let zero        = Zero
let unit        = Unit
let sym x       = Sym x
let many        = function
                | Unit        -> Unit
                | Zero        -> Unit
                | x             -> Many x
let some        = function
                | Unit        -> Unit
                | Zero        -> Zero
                | Many x | Opt x | Some x | x  -> Some x
let opt         = function
                | (Opt _ | Many _ | Unit)  as v-> v
                | Zero          -> Unit
                | x             -> Opt x
let seq x y     = match (x,y) with
                | Zero, x     -> Zero
                | Unit, x     -> x
                | x     , Zero-> Zero
                | x     , Unit-> x
                | x     , y     -> Seq(x,y)
let alt x y     = match (x,y) with
                | Zero, x
                | x     , Zero  -> x
                | x     , Unit
                | Unit  , x     -> opt x
                | x     , y     -> Alt(x,y)


 (** Parse regexp's with a precedence climbing parser, we are going right to
    left because regexps use postfix notation... *)
module Parser = struct
  type token =
    | Zero of rx
    | One of (rx -> rx)
    | Two of int * (rx -> rx -> rx)
    | Opar
    | Cpar

  (* Returns a reverse token string
     We introduce the seq tokens that are implicitely between tokens here
  *)
  let rec tokenize s pos last_was_primary acc =
    if pos >= String.length s then
      acc
    else
      let next last_was_primary acc = tokenize s (pos+1) last_was_primary acc in
      match s.[pos] with
        | '('  when last_was_primary -> next false (Opar::Two (2,seq)::acc)
        | '(' -> next false (Opar::acc)
        | ')'  -> next true (Cpar::acc)
        | '*'  -> next true (One many::acc)
        | '+'  -> next true (One some::acc)
        | '?'  -> next true (One opt::acc)
        | '|'  -> next false (Two (1,alt)::acc)
        | c when last_was_primary  -> next true (Zero (Sym c)::Two (2,seq)::acc)
        | c -> next true (Zero (Sym c)::acc)

  let tokenize s = tokenize s 0 false []

  (* http://en.wikipedia.org/wiki/Operator-precedence_parser *)
  let rec parse (rhs:rx) (min_prec:int) = function
    | Two (prec,f)::rest when prec >= min_prec ->
      let lhs,rest = parse_primary rest in
      let lhs,rest = parse lhs prec rest in
      parse (f lhs rhs) min_prec rest
    | l -> rhs,l
  and parse_primary = function
    | Zero rx::rest -> rx,rest
    | One f::rest ->
      let rx,rest = parse_primary rest in
      f rx,rest
    | Cpar::rest ->
      let rx,rest = parse_primary rest in
      let rx,rest = parse rx (-1) rest in
      begin
        match rest with
          | Opar::rest -> rx,rest
          | _ -> assert false
      end
    | _ -> assert false

  let parse s =
    let toks = tokenize s in
    let prim,toks = parse_primary toks in
    fst (parse prim 0 toks)
end
let of_string = Parser.parse

let ( ||| ) = alt
let ( *** ) = seq

(*
[nullable e] is true, iff the empty sequence ([Zero]) is recognized by [e].
*)
let rec nullable = function
    | Zero            -> false
    | Unit            -> true
    | Sym x           -> false
    | Many e          -> true
    | Some e          -> nullable e
    | Opt e           -> true
    | Seq(e1,e2)      -> nullable e1 && nullable e2
    | Alt(e1,e2)      -> nullable e1 || nullable e2

(* [residual e x] returns a regular expression [[e']] that recognizes the
language $L(e') = \{ w | xw \in L(e)\}$. *)
let rec residual e' x = match e' with
    | Zero            -> Zero
    | Unit            -> Zero
    | Sym x'          -> if   x' = x
                           then Unit
                           else Zero
    | Many e          -> seq (residual e x) (many e)
    | Some e          -> seq (residual e x) (many e)
    | Opt e           -> residual e x
    | Seq(e1,e2)      -> if   nullable e1
                           then alt (seq (residual e1 x) e2) (residual e2 x)
                           else seq (residual e1 x) e2
    | Alt(e1,e2)      -> alt (residual e1 x) (residual e2 x)

(* [[matches e syms]] is true, iff the word [[syms]] is an element of $L(e)$,
   i.e.  [[e]] matches the symbols [[syms]] *)

let matches e syms      = nullable (List.fold_left residual e syms)

(*[matchstr e str] is true, iff string [str] is matched by
  regular expression [e]. *)
let matchstr e str =
    let len = String.length str         in
    let rec loop e i =
        if   i = len
        then nullable e
        else loop (residual e (String.get str i)) (i+1)
    in
        loop e 0

let (=~) str re =
  let len = String.length str         in
  let rec loop e i =
    print_endline (to_string e);
    if   i = len
    then nullable e
    else loop (residual e (String.get str i)) (i+1)
  in
  loop (of_string re) 0

let _ = "abaaab" =~ "a+b|ab*a"

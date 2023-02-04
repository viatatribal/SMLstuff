(* This is a brainfuck interpreter *)
(* !!! <- means that it needs to be rewritten *)

use "assert.sml";

datatype bfdt =
         Right
       | Left
       | Increment
       | Decrement
       | Output
       | Input
       | Jumpforward
       | Jumpbackward;


type code = char list;
type cells = int list;
type pointer = int;
type bfcode = bfdt list;
(* the stack is a pair of bfdt and an int
 * where the int represents how deep we are in the
 * loops *)
type stack = (int * bfdt) list;
type brainfuck = cells * pointer * bfcode;

(* for now the cells will be a list of size 100 *)
val cellSize = 100;
(* our cell "array" *)
val cellArray = List.tabulate (cellSize, fn x => 0) : cells;
(* our initial brainfuck *)
val brainfuck = (cellArray, 0, []) : brainfuck;
(* pointer to the cell array *)
val stack = [] : stack;

(* code -> bfcode
 * parser our code into a bfcode *)
fun parser [] = [] : bfdt list
  | parser ((x::xs): code) =
    case x of
       #">" => Right        :: parser xs
     | #"<" => Left         :: parser xs
     | #"+" => Increment    :: parser xs
     | #"-" => Decrement    :: parser xs
     | #"." => Output       :: parser xs
     | #"," => Input        :: parser xs
     | #"[" => Jumpforward  :: parser xs
     | #"]" => Jumpbackward :: parser xs
     | _    =>                 parser xs;

(* tests *)
Assert.trueList (parser (String.explode (">><<[]"))) [Right, Right, Left, Left, Jumpforward, Jumpbackward];
Assert.trueList (parser (String.explode ("<<"))) [Left, Left];
Assert.trueList (parser (String.explode ("><+-.,[]\n"))) [Right, Left, Increment, Decrement,
                                                            Output, Input, Jumpforward, Jumpbackward];

(* bfcode  stack  int -> bfcode * stack
 * return the values in the stack back to bfcode *)
fun jumpb (bfs, (l,b)::st, lp) =
    case b of
        Jumpforward => if l = lp then (bfs, (l, b)::st)
                       else jumpb (b::bfs, st, lp)
      | _ => jumpb (b::bfs, st, lp);

(* bfcode  stack int int -> bfcode * stack
 * sends vales from bfcode into the stack *)
fun jumpf (b::bfs, st, lp, clp) =
    case b of
        Jumpforward  => jumpf (bfs, (lp+1, b)::st, lp+1, clp)
      | Jumpbackward => if lp = clp then (bfs, (lp,b)::st)
                        else jumpf (bfs, (lp,b)::st, lp-1, clp)
      | _ => jumpf (bfs, (lp,b)::st, lp, clp);


(* char -> unit
 * Outputs a char *)
fun outputChar c =
    TextIO.output(TextIO.stdOut, String.str(c));


(* unit -> int
 * Reads a input from user and convert it into int *)
fun inputInt () =
    let val c = valOf (TextIO.inputLine(TextIO.stdIn))
    in
      case Int.fromString c of
          SOME(x) => x
        | NONE    =>
          Char.ord (hd (String.explode c))
    end;


(* brainfuck stack int -> brainfuck
 * evaluate our bfcode
 * and returns the current state of cells, pointer and
 * bfcode cleared
 * int is used to tell us what level of the brainfuck loop are we
 * where 0 means no loop *)
fun eval (((xs, pf, []) : brainfuck), st, lp) = ( outputChar #"\n"
                                                ; (xs, pf, []) : brainfuck
                                                )
  | eval ((xs, pf, b::bfs), st, lp) =
    case b of
        Right => eval ((xs, pf+1, bfs), (lp,b)::st, lp)
      | Left => eval ((xs, pf-1, bfs), (lp,b)::st, lp)
      | Increment => eval ((List.update (xs, pf, List.nth (xs, pf) + 1), pf, bfs), (lp, b)::st, lp)
      | Decrement => eval ((List.update (xs, pf, List.nth (xs, pf) - 1), pf, bfs), (lp, b)::st, lp)
      | Output    => ( outputChar (Char.chr(List.nth (xs, pf)))
                     ; eval ((xs, pf, bfs), (lp,b)::st,lp)
                     )
      | Input => let val c = inputInt ()
                 in eval ((List.update (xs, pf, c), pf, bfs), (lp, b)::st, lp)
                 end
      | Jumpforward => if List.nth(xs, pf) = 0
                       then let val (bfs', st') = jumpf(bfs, (lp, b)::st, lp, lp)
                            in  eval ((xs, pf, bfs'), st', lp-1)
                            end
                       else eval ((xs, pf, bfs), (lp+1, b)::st, lp+1)
      | Jumpbackward => if (List.nth (xs, pf)) = 0
                        then eval ((xs, pf, bfs), (lp, b)::st, lp-1)
                        else let val (bfs',st') = jumpb(bfs, (lp, b)::st, lp)
                             in eval ((xs, pf, bfs'), st', lp)
                             end;


(* unit -> string
 * Reads a brainfuck code from user *)
fun bfinput () =
    ( TextIO.output(TextIO.stdOut, "Brainfuck: ")
    ; TextIO.flushOut(TextIO.stdOut)
    ; valOf (TextIO.inputLine(TextIO.stdIn))
    );


(* brainfuck bfcode -> brainfuck
 * update brainfuck *)
fun updatebf ((xs, pf, bfs), bfs') =
    (xs, pf, bfs') : brainfuck;


(* unit -> unit
 * the main loop of the interpreter
 * stops when the user enters "Exit" *)
fun interpreter () =
    let
      fun inter' bfcode bf =
          if bfcode = "Exit\n" then ()
          else
            let val bfcode = parser(String.explode (bfcode))
                val bf = updatebf (bf, bfcode)
                val bf = eval (bf, stack, 0)
            in
              inter' (bfinput ()) bf
            end
    in
      inter' (bfinput ()) brainfuck
    end;


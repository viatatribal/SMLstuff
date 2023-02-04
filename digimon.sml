use "assert.sml";
use "digimondata.sml";


(* string -> string *)
(* removes 2/3 initials value from a string *)
(* it's used to remove the inital "xx." characters *)
fun strprefrem str =
    if String.sub (str, 2) = #"."
    then String.implode (List.drop (String.explode str, 3))
    else String.implode (List.drop (String.explode str, 2));

(* tests *)
Assert.equalString (strprefrem "1.Agumon") "Agumon";
Assert.equalString (strprefrem "2.Leomon") "Leomon";
Assert.equalString (strprefrem "11.Greymon") "Greymon";


(* status1{1,2} -> string *)
(* returns a string without any "" from status1 or status2 *)
fun clearst ((a,b,c,d,e,f)) =
    String.concat
        (List.filter (fn x => x <> "\n")
                     (List.map (fn x => x ^ "\n") [a,b,c,d,e,f]));

(* tests *)
Assert.equalString (clearst("","","","","",""))  "";
Assert.equalString (clearst("","100","30","","","")) "100\n30\n";
Assert.equalString (clearst("1","1", "1", "1", "1","1")) "1\n1\n1\n1\n1\n1\n";


(* digimon -> string *)
(* returns the string formed from a digimon requirements *)
fun reqstring ((st1,st2,st3) : digimon) =
    let val str = clearst (st1) ^ clearst (st2)
    in
      if st3 = "" then str
      else str ^ st3
    end;

(* tests *)
Assert.equalString (reqstring((("","","","","","")
                  ,("Care Mistakes: >=0"
                  ,"Weigth: -5 to 5", "","","","")
                  ,"")))
                   "Care Mistakes: >=0\nWeigth: -5 to 5\n";
Assert.equalString (reqstring((("","","","Defense: >=1","Speed: >=1","Brains: >=1")
                  ,("Care Mistakes: >=0","Weight: 10 to 20","","","","")
                  ,"")))
                   "Defense: >=1\nSpeed: >=1\nBrains: >=1\nCare Mistakes: >=0\nWeight: 10 to 20\n";
Assert.equalString (reqstring((("HP: >=10", "MP: >=10", "Offense: >=1","","","")
                 ,("Care Mistakes: >=0", "Weight: 10 to 20", "","","","")
                 ,"")))
                   "HP: >=10\nMP: >=10\nOffense: >=1\nCare Mistakes: >=0\nWeight: 10 to 20\n";


(* digilevel -> string *)
(* returns a string from a digilevel *)
fun levelstring x =
    let val str = (String.concat o (List.map (fn x => x ^ "\n")))
    in
      case x of
          Fresh(y)    => "Fresh:\n" ^ str y
        | Training(y) => "Training:\n" ^ str y
        | Rookie(y)   => "Rookie:\n" ^ str y
        | Champion(y) => "Champion:\n" ^ str y
        | Ultimate(y) => "Ultimate:\n" ^ str y
                   end;

(* tests *)
Assert.equalString (levelstring fresh) "Fresh:\n1.Yuramon\n2.Poyomon\n3.Punimon\n4.Puramon\n";
Assert.equalString (levelstring rookie) ("Rookie:\n1.Agumon\n2.Gabumon\n3.Kunemon\n4.Elecmon\n"
                                      ^ "5.Penguinmon\n6.Palmon\n7.Betamon\n8.Patamon\n9.Biyomon\n");

(* digilevel -> list *)
(* returns a list from a digilevel *)
fun levellist dl =
    case dl of
       Fresh(x)    => x
     | Training(x) => x
     | Rookie(x)   => x
     | Champion(x) => x
     | Ultimate(x) => x;

(* tests *)
Assert.equalListString (levellist fresh) ["1.Yuramon", "2.Poyomon", "3.Punimon", "4.Puramon"];
Assert.equalListString (levellist rookie) ["1.Agumon", "2.Gabumon", "3.Kunemon", "4.Elecmon"
                                    ,"5.Penguinmon","6.Palmon", "7.Betamon", "8.Patamon", "9.Biyomon"];


(* list string -> string *)
(* returns a string from a list of string *)
fun evostring x =
    String.concat (List.map (fn x => x ^ "\n") x);

(* tests *)
Assert.equalString (evostring ["Agumon","Patamon","Bakemon"]) "Agumon\nPatamon\nBakemon\n";
Assert.equalString (evostring [""]) "\n";
Assert.equalString (evostring ["Agumon"]) "Agumon\n";


(* unit -> unit *)
(* prints explanation of requirements *)
fun help () =
    ( TextIO.output(TextIO.stdOut, "You only need to satisfy 3 of the 4 properties:\n")
    ; TextIO.output(TextIO.stdOut, "1. HP-MP-Offense-Defense-Speed-Brains.\n")
    ; TextIO.output(TextIO.stdOut, "2. Care Mistakes.\n")
    ; TextIO.output(TextIO.stdOut, "3. Weight.\n")
    ; TextIO.output(TextIO.stdOut, "4. Discipline-Techniques-Battles-Happines-Extra Requirement.\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> unit *)
(* prints the main screen of the program *)
fun prelude () =
    ( TextIO.output(TextIO.stdOut,"Digimon Evo Tool!\n")
    ; TextIO.output(TextIO.stdOut,"This program shows the requirement to digivolve")
    ; TextIO.output(TextIO.stdOut," one digimon to another!\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> unit *)
(* prints an error indicating wrong number *)
fun errormsg (min, max) =
    ( TextIO.output(TextIO.stdOut, "Wrong number.\n")
    ; TextIO.output(TextIO.stdOut, "Must be a number between ")
    ; TextIO.output(TextIO.stdOut, Int.toString min)
    ; TextIO.output(TextIO.stdOut, " and ")
    ; TextIO.output(TextIO.stdOut, Int.toString max)
    ; TextIO.output(TextIO.stdOut, "\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> digilevel * int *)
(* user input the level [by number] he wants *)
fun levelselector () =
    case valOf (TextIO.inputLine(TextIO.stdIn)) of
       "1\n" => (fresh, 4)
     | "2\n" => (training, 4)
     | "3\n" => (rookie, 9)
     | "4\n" => (champion, 30)
     | "5\n" => (ultimate, 15)
     |  _    => ( errormsg(1,5); levelselector());


(* int int -> int *)
(* user input the digimon he wants between numbers min and max *)
fun pickdigimon (min, max) =
    let val n =
            valOf(Int.fromString(valOf(TextIO.inputLine(TextIO.stdIn))))
    in
      if n < min orelse n > max
      then (errormsg(min,max); pickdigimon (min,max))
      else n
    end;


(* unit -> unit *)
(* asks the user to select one of levels *)
fun whatlevel () =
    ( TextIO.output(TextIO.stdOut, "Select a level (enter a number):\n")
    ; TextIO.output(TextIO.stdOut, "1.Fresh\n2.Training\n3.Rookie\n")
    ; TextIO.output(TextIO.stdOut, "4.Champion\n5.Ultimate\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* string -> unit *)
(* asks the user what digimon he wants *)
fun whatdigimon (str,digistr) =
    ( TextIO.output(TextIO.stdOut, str)
    ; TextIO.output(TextIO.stdOut, digistr)
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* string -> unit *)
(* prints the requirement *)
fun displayreq digi =
    ( TextIO.output(TextIO.stdOut, "The requirements are: \n")
    ; TextIO.output(TextIO.stdOut, digi)
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> unit *)
(* asks the user if he wants to end the program *)
fun quit () =
    ( TextIO.output(TextIO.stdOut, "Do you want to close digievotool?\n")
    ; TextIO.output(TextIO.stdOut, "Yes or No\n")
    ; TextIO.flushOut(TextIO.stdOut)
    );


(* unit -> bool *)
(* user inputs if he wants to leave or not *)
fun exitanswer () =
    case valOf (TextIO.inputLine(TextIO.stdIn)) of
        "Yes\n" => true
      | "yes\n"  => true
      | "no\n" => false
      | "No\n" => false
      | _ =>
        ( TextIO.output(TextIO.stdOut, "Please answer Yes or No.\n")
        ; exitanswer());


(* unit -> unit *)
(* main function of the whole tool *)
fun digievotool () =
    let
      fun A () : unit = ( prelude(); help(); whatlevel(); B() )

      and B () = let
        val (dg, num) = levelselector ()
        val dgstr = levelstring dg
      in
        ( whatdigimon ("Select a digimon (enter a number):\n", dgstr)
        ; C(num, levellist dg)
        )
      end

      and C (num, diglst) = let
        val dg = List.nth (diglst, pickdigimon (1, num)-1)
        val evol = Table.getval(strprefrem dg, evotable)
        val dgstr = evostring evol
      in
        ( whatdigimon ("Select an evolution (enter a number):\n", dgstr)
        ; D(num, evol))
      end

      and D (num, diglst) = let
        val dg = List.nth (diglst, pickdigimon(1, num)-1)
        val reql = Table.getval(strprefrem dg, reqtable)
        val reqstr = reqstring reql
      in
        (displayreq (reqstr)
        ; quit()
        ; E()
        )
      end

      and E () =
          if exitanswer() then ()
          else digievotool()
    in
      A ()
    end;


use "table.sml";
(* datatype for the level of digimons *)
datatype digilevel = Fresh of string list
                   | Training of string list
                   | Rookie of string list
                   | Champion of string list
                   | Ultimate of string list;

type hp          = string;
type mp          = string;
type offense     = string;
type defense     = string;
type speed       = string;
type brains      = string;
type status1     = (hp * mp * offense * defense * speed * brains);

type caremistake = string;
type weight      = string;
type discipline  = string;
type techniques  = string;
type battles     = string;
type happiness   = string;
type status2     = (caremistake * weight *discipline * techniques * battles * happiness);

type extrarequirement = string;
type digimon = (status1 * status2 * extrarequirement);

(* list of digimons per level *)
val fresh = Fresh(["1.Yuramon", "2.Poyomon", "3.Punimon", "4.Puramon"]);
val training = Training(["1.Koromon", "2.Tsunomon", "3.Tokomon", "4.Tanemon"]);
val rookie = Rookie(["1.Agumon", "2.Gabumon", "3.Kunemon", "4.Elecmon", "5.Penguinmon",
                     "6.Palmon", "7.Betamon", "8.Patamon", "9.Biyomon"]);
val champion = Champion(["1.Birdramon", "2.Meramon", "3.Greymon", "4.Tyrannomon", "5.Monochromon",
                         "6.Centarumon", "7.Garurumon", "8.Ogremon", "9.Drimogemon", "10.Sukamon",
                         "11.Nanimon", "12.Vademon", "13.Bakemon", "14.Numemon", "15.Whamon", "16.Frigimon",
                         "17.Shellmon", "18.Mojyamon", "19.Kokatorimon", "20.Leomon", "21.Angemon",
                         "22.Unimon", "23.Airdramon", "24.Kabuterimon", "25.Seadramon", "26.Coelamon",
                         "27.Vegiemon", "28.Kuwagamon", "29.Ninjamon", "30.Devimon"]);
val ultimate = Ultimate(["1.Phoenixmon", "2.Megadramon", "3.Skullgreymon", "4.MetalGreymon",
                         "5.Andromon", "6.MetalMamemon", "7.Giromon", "8.MegaSeadramon", "9.Mamemon",
                         "10.HKabuterimon", "11.Piximon", "12.Monzaemon", "14.Etemon", "15.Digitamamon"]);


(* table of requirements *)
val reqtable =
  let
    (* digimon name - status requirement *)
    val Koromon = (("","","","","","")
                  ,("Care Mistakes: >=0"
                  ,"Weigth: -5 to 5", "","","","")
                  ,"") : digimon
    val Tsunomon = (("","","","","","")
                   ,("Care Mistakes: >=0"
                   ,"Weigth: -5 to 5", "","","","")
                   ,"") : digimon
    val Tanemon = (("","","","","","")
                  ,("Care Mistakes: >=0"
                  ,"Weigth: -5 to 5", "","","","")
                  ,"") : digimon
    val Tokomon = (("","","","","","")
                  ,("Care Mistakes: >=0"
                  ,"Weigth: -5 to 5", "","","","")
                  ,"") : digimon
    val Agumon = (("HP: >=10", "MP: >=10", "Offense: >=1","","","")
                 ,("Care Mistakes: >=0", "Weight: 10 to 20", "","","","")
                 ,"") : digimon
    val Gabumon = (("","","","Defense: >=1","Speed: >=1","Brains: >=1")
                  ,("Care Mistakes: >=0","Weight: 10 to 20","","","","")
                  ,"") : digimon
    val Kunemon = (("","","","","","")
                  ,("","","","","","")
                  ,"Sleep in Kunemon's bed. 50% chances.") : digimon
    val Patamon = (("HP: >=10","","Offense: >=1","","","Brains: >=1")
                  ,("Care Mistakes: >=0","Weigth: 10 to 20","","","","")
                  ,"") : digimon
    val Biyomon = (("HP: >=10","","","Defense: >=1","Speed: >=1","")
                  ,("Care Mistakes: >=0","Weigth: 10 to 20","","","","")
                  ,"") : digimon
    val Elecmon = (("HP: >=10","","Offense: >=1","","Speed: >=1","")
                  ,("Care Mistakes: >=0","Weigth: 10 to 20","","","","")
                  ,"") : digimon
    val Penguinmon = (("","MP: >=10","","Defense: >=1","","Brains: >=1")
                     ,("Care Mistakes: >=0","Weigth: 10 to 20","","","","")
                     ,"") : digimon
    val Palmon = (("","MP: >=10","","","Speed: >=1","Brains: >=1")
                 ,("Care Mistakes: >=0","Weigth: 10 to 20","","","","")
                 ,"") : digimon
    val Betamon = (("HP: >=10","MP: >=10","","Defense: >=1","","")
                  ,("Care Mistakes: >=0","Weigth: 10 to 20","","","","")
                  ,"") : digimon
    val Birdramon = (("","","","","Speed: >= 100","")
                    ,("Care Mistakes: >= 5","Weigth: 15 to 25","","Techniques: >= 28","Battles: >= 10","")
                    ,"Have Biyomon as Partner") : digimon
    val Meramon = (("","","Offense: >=100","","","")
                  ,("Care Mistakes: >=5","Weigth: 15 to 25","","Techniques: >=28","Battles: >=10","")
                  ,"") : digimon
    val Greymon = (("","","Offense: >=100","Defense: >=100","Speed: >=100","Brains: >=100")
                  ,("Care Mistakes: <=1","Weigth: 25 to 35","Discipline: >=90","Techniques: >=35","","")
                  ,"") : digimon
    val Tyrannomon = (("HP: >=1000","","","Defense: >=100","","")
                     ,("Care Mistakes: <=5","Weigth: 25 to 35","","Techniques: >=28","Battles: <=5","")
                     ,"") : digimon
    val Monochromon = (("HP: >=1000","","","Defense: >=100","","Brains: >=100")
                      ,("Care Mistakes: <=3","Weigth: 35 to 45","","Techniques: >=35","Battles: <=5","")
                      ,"") : digimon
    val Centarumon = (("","","","","","Brains: >=100")
                     ,("Care Mistakes: <=3","Weigth: 25 to 35","Discipline: >=60","Techniques: >=28","","")
                     ,"") : digimon
    val Drimogemon =  (("","","Offense: >=100","","","")
                      ,("Care Mistakes: >=3","Weigth: 35 to 45","","Techniques: >=28","","Happiness: >=50")
                      ,"") : digimon
    val Ogremon = (("HP: >=1000","","Offense: >=100","","","")
                  ,("Care Mistakes: >=5","Weigth: 25 to 35","","Techniques: >=35","Battles: >=15","")
                  ,"") : digimon
    val Garurumon = (("","MP: >=1000","","","Speed: >=100","")
                    ,("Care Mistakes: <=1","Weigth: 25 to 35","Discipline: >=90","Techniques: >=28","","")
                    ,"") : digimon
    val Angemon = (("","MP: >=1000","","","","Brains: >=100")
                  ,("Care Mistakes: <=0","Weigth: 15 to 25","","Techniques: >=35","","")
                  ,"Extra Requirement: Have Patamon as Partner") : digimon
    val Leomon = (("","","Offense: >=100","","Speed: >=100","Brains: >=100")
                 ,("Care Mistakes: <=1","Weigth: 15 to 25","","Techniques: >=35","Battles: >=10","")
                 ,"") : digimon
    val Unimon = (("HP: >=1000","","","","Speed: >=100","")
                 ,("Care Mistakes: <=3","Weigth: 25 to 35","","Techniques: >=35","Battles: >=10","")
                 ,"") : digimon
    val Kokatorimon = (("HP: >=1000","","","","","")
                      ,("Care Mistakes: >=3","Weigth: 25 to 34","","Techniques: >=28","","")
                      ,"Have Biyomon as Partner") : digimon
    val Bakemon = (("","MP: >=1000","","","","")
                  ,("Care Mistakes: >=3","Weigth: 15 to 25","","Techniques: >=28","","Happiness: >=50")
                  ,"Extra Requirement: Not Penguinmon. Die. 10% Chance") : digimon
    val Airdramon = (("","MP: >=1000","","","Speed: >=100","Brains: >100")
                    ,("Care Mistakes: <=1","Weigth: 25 to 35","Discipline: >=90","Techniques: >=35","","")
                    ,"") : digimon
    val Kabuterimon = (("HP: >=1000","MP: >=1000","Offense: >=100","","Speed: >=100","")
                      ,("Care Mistakes: <=1","Weigth: 25 to 35","","Techniques: >=35","","")
                      ,"Extra Requirement: Have Kunemon as Partner") : digimon
    val Vegiemon = (("","MP: >=1000","","","","")
                   ,("Care Mistakes: >=5","Weigth: 5 to 15","","Techniques: >=21","","Happiness: >=50")
                   ,"") : digimon
    val Kuwagamon = (("HP: >=1000","MP: >=1000","Offense: >=100","","Speed: >=100","")
                    ,("Care Mistakes: >=5","Weigth: 25 to 35","","Techniques: >=28","","")
                    ,"Extra Requirement: Have Kunemon as Partner") : digimon
    val Whamon = (("HP: >=1000","","","","","Brains: >=100")
                 ,("Care Mistakes: <=5","Weigth: 35 to 45","Discipline: >=60","Techniques: >=28","","")
                 ,"") : digimon
    val Coelamon = (("","","","Defense: >=100","","")
                   ,("Care Mistakes: >=3","Weigth: 25 to 35","","Techniques: >=35","Battles: <=5","")
                   ,"") : digimon
    val Ninjamon = (("","MP: >=1000","Offense: >=100","","Speed: >=100","")
                   ,("Care Mistakes: <=1","Weigth: 5 to 15","","Techniques: >=35","Battles: >=15","")
                   ,"") : digimon
    val Shellmon = (("HP: >=1000","","","Defense: >=100","","")
                   ,("Care Mistakes: >=5","Weigth: 35 to 45","","Techniques: >=35","","")
                   ,"Extra Requirement: Have Betamon as Partner") : digimon
    val Seadramon = (("HP: >=1000","MP: >=1000","","","","")
                    ,("Care Mistakes: >=3","Weigth: 25 to 35","","Techniques: >=28","Battles: <=5","")
                    ,"") : digimon
    val Mojyamon = (("HP: >=1000","","","","","")
                   ,("Care Mistakes: >=5","Weigth: 15 to 25","","Techniques: >=28","Battles: <=5","")
                   ,"") : digimon
    val Frigimon = (("","MP: >=1000","","","","Brains: >=100")
                   ,("Care Mistakes: <=5","Weigth: 25 to 35","","Techniques: >=28","","Happiness: >=50")
                   ,"") : digimon
    val Devimon = (("","","","","","")
                  ,("","","Discipline: <= 50%","","","")
                  ,"Extra Requirement: Die as Angemon and you'll have a chance to evolve") : digimon
    val Numemon = (("","","","","","")
                  ,("","","","","","")
                  ,"Have an Evolution Counter of 96 hours") : digimon
    val Nanimon = (("","","","","","")
                  ,("","","Discipline: <=0","","","Happiness: <=0")
                  ,"Extra Requirement: Scold a digimon") : digimon
    val MetalGreymon = (("HP: >=4000","MP: >=3000","Offense: >=500","Defense: >=500","Speed: >=300","Brains: >=300")
                       ,("Care Mistakes: <=10","Weigth: 60 to 70","Discipline: >=95","Techniques: >=30","Battles: >=30","")
                       ,"") : digimon
    val SkullGreymon = (("HP: >=4000","MP: >=6000","Offense: >=400","Defense: >=400","Speed: >=200","Brains: >=500")
                       ,("Care Mistakes: >=10","Weigth: 25 to 35","","Techniques: >=45","Battles: >=45","")
                       ,"") : digimon
    val MetalMamemon = (("","","Offense: >=500","Defense: >=400","Speed: >=400","Brains: >=400")
                       ,("Care Mistakes: <=15","Weigth: 5 to 15","","Techniques: >=30","","Happiness: >=95")
                       ,"") : digimon
    val Andromon = (("HP: >=2000","MP: >=4000","Offense: >=200","Defense: >=400","Speed: >=200","Brains: >=400")
                   ,("Care Mistakes: <=5","Weigth: 35 to 45","Discipline: >=95","Techniques: >=30","Battles: >=30","")
                   ,"") : digimon
    val Giromon = (("","","Offense: >=400","","Speed: >=300","Brains: >=400")
                  ,("Care Mistakes: >=15","Weigth: 0 to 10","","Techniques: >=35","Battles: >=100","Happiness: >=95")
                  ,"") : digimon
    val Megadramon = (("HP: >=3000","MP: >=5000","Offense: >=500","Defense: >=300","Speed: >=400","Brains: >=400")
                     ,("Care Mistakes: <=10","Weigth: 50 to 60","","Techniques: >=30","Battles: >=30","")
                     ,"") : digimon
    val Phoenixmon = (("HP: >=4000","MP: >=4000","","","Speed: >=400","Brains: >=600")
                     ,("Care Mistakes: <=3","Weigth: 25 to 35","Discipline: >=100","Techniques: >=40","Battles: <=0","")
                     ,"") : digimon
    val HKabuterimon = (("HP: >=7000","","Offense: >=400","Defense: >=600","Speed: >=400","")
                       ,("Care Mistakes: <=5","Weigth: 50 to 60","","Techniques: >=40","Battles: <=0","")
                       ,"") : digimon
    val Piximon = (("","","Offense: >=300","Defense: >=300","Speed: >=400","Brains: >=400")
                  ,("Care Mistakes: >=15","Weigth: 0 to 10","","Techniques: >=25","","Happiness: >=95")
                  ,"") : digimon
    val MegaSeadramon = (("","MP: >=4000","Offense: >=500","Defense: >=400","","Brains: >=400")
                        ,("Care Mistakes: <=5","Weigth: 25 to 35","","Techniques: >=40","Battles: <=0","")
                        ,"") : digimon
    val Mamemon = (("","","Offense: >=400","Defense: >=300","Speed: >=300","Brains: >=400")
                  ,("Care Mistakes: >=15","Weigth: 0 to 10","","Techniques: >=25","","Happiness: >=90")
                  ,"") : digimon
    val Digitamamon = (("HP: >=3000","MP: >=3000","Offense: >=400","Defense: >=400","Speed: >=400","Brains: >=300")
                      ,("Care Mistakes: <=0","Weigth: 5 to 15","","Techniques: >=49","Battles: >=100","")
                      ,"") : digimon
    val Monzaemon = (("HP: >=3000","MP: >=3000","Offense: >=300","Defense: >=300","Speed: >=300","Brains: >=300")
                    ,("Care Mistakes: <=0","Weigth: 35 to 45","","Techniques: >=49","Battles: >=50","")
                    ,"Extra Requirement: Talk to the Monzaemon Suit before beating WaruMonzaemon") : digimon
    val Etemon = (("HP: >=2000","MP: >=3000","Offense: >=400","Defense: >=200","Speed: >=400","Brains: >=300")
                 ,("Care Mistakes: <=0","Weigth: 10 to 20","","Techniques: >=49","Battles: >=50","")
                 ,"") : digimon
    val Vademon =  (("","","","","","")
                   ,("","","","","","")
                   ,"Extra Requirement: Praise/Scold with an Evolution Counter of at least 240 hours\n"
                    ^ "50% chance gets forced at 360 hours") : digimon;
    val Sukamon = (("","","","","","")
                  ,("","","","","","")
                  ,"Extra Requirement: Get a Full Virus bar") : digimon
  (* list of pairs (name - requirement) to turn into a table *)
    val reqlist = [("Koromon",Koromon), ("Tsunomon",Tsunomon), ("Tanemon",Tanemon), ("Tokomon",Tokomon)
                  ,("Agumon",Agumon), ("Gabumon",Gabumon), ("Kunemon",Kunemon), ("Patamon",Patamon)
                  ,("Biyomon",Biyomon), ("Elecmon",Elecmon), ("Penguinmon",Penguinmon), ("Palmon",Palmon)
                  ,("Betamon",Betamon), ("Birdramon",Birdramon), ("Meramon",Meramon), ("Greymon",Greymon)
                  ,("Tyrannomon",Tyrannomon), ("Monochromon",Monochromon), ("Centarumon",Centarumon)
                  ,("Drimogemon",Drimogemon), ("Ogremon",Ogremon), ("Garurumon",Garurumon), ("Angemon",Angemon)
                  ,("Leomon",Leomon), ("Unimon",Unimon), ("Kokatorimon",Kokatorimon), ("Bakemon",Bakemon)
                  ,("Airdramon",Airdramon), ("Kabuterimon",Kabuterimon), ("Vegiemon",Vegiemon), ("Kuwagamon",Kuwagamon)
                  ,("Whamon",Whamon), ("Coelamon",Coelamon), ("Ninjamon",Ninjamon), ("Shellmon",Shellmon)
                  ,("Seadramon",Seadramon), ("Mojyamon",Mojyamon), ("Frigimon",Frigimon), ("Devimon",Devimon)
                  ,("Numemon",Numemon), ("Nanimon",Nanimon), ("MetalGreymon",MetalGreymon), ("SkullGreymon",SkullGreymon)
                  ,("MetalMamemon",MetalMamemon), ("Andromon",Andromon), ("Giromon",Giromon), ("Megadramon",Megadramon)
                  ,("Phoenixmon",Phoenixmon), ("HKabuterimon",HKabuterimon), ("Piximon",Piximon), ("MegaSeadramon",MegaSeadramon)
                  ,("Mamemon",Mamemon), ("Digitamamon",Digitamamon), ("Monzaemon",Monzaemon), ("Etemon",Etemon)
                  ,("Vademon",Vademon), ("Sukamon",Sukamon)]
  in
    Table.fromList reqlist
  end;

(* table of evolution *)
val evotable =
  let
    (* digievolutions *)
    (* digimon -> list of evolutions *)
    val botamon = ["1.Koromon"]
    val koromon = ["1.Agumon","2.Gabumon","3.Kunemon"]
    val agumon = ["1.Birdramon","2.Meramon","3.Greymon","4.Tyrannomon",
                  "5.Monochromon","6.Centarumon","7.Sukamon","8.Numemon","9.Bakemon","10.Nanimon"]
    val gabumon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Bakemon","5.Drimogemon","6.Ogremon"
                   ,"7.Garurumon","8.Centarumon","9.Monochromon","10.Tyrannomon"]
    val punimon = ["1.Tsunomon"]
    val tsunomon = ["1.Elecmon","2.Penguinmon","3.Kunemon"]
    val elecmon = ["1.Angemon","2.Leomon","3.Kokatorimon","4.Bakemon","5.Numemon","6.Sukamon","7.Nanimon"]
    val penguinmon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Garurumon","5.Mojyamon","6.Shellmon"
                      ,"7.Frigimon","8.Whamon", "9.Bakemon"]
    val poyomon = ["1.Tokomon"]
    val tokomon = ["1.Patamon","2.Biyomon","3.Kunemon"]
    val patamon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Bakemon","5.Angemon","6.Leomon","7.Ogremon"
                   ,"8.Drimogemon","9.Tyrannomon","10.Unimon"]
    val biyomon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Bakemon","5.Unimon","6.Birdramon","7.Airdramon"
                   ,"8.Kabuterimon","9.Kokatorimon"]
    val yuramon = ["1.Tanemon"]
    val tanemon = ["1.Palmon","2.Betamon","3.Kunemon"]
    val palmon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Bakemon","5.Whamon","6.Coelamon","7.Vegiemon","8.Kuwagamon", "9.Ninjamon"]
    val betamon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Bakemon","5.Whamon","6.Coelamon","7.Shellmon","8.Seadramon"]
    val kunemon = ["1.Nanimon","2.Sukamon","3.Numemon","4.Bakemon","5.Kabuterimon","6.Kuwagamon","7.Vegiemon"]
    val birdramon = ["1.Phoenixmon","2.Vademon","3.Sukamon"]
    val airdramon = ["1.Phoenixmon","2.Megadramon","3.Vademon","4.Sukamon"]
    val phoenixmon = ["1.Sukamon"]
    val megadramon = ["1.Sukamon"]
    val meramon = ["1.MetalGreymon","2.Andromon","3.Vademon","4.Sukamon"]
    val metalgreymon = ["1.Sukamon"]
    val skullgreymon = ["1.Sukamon"]
    val greymon = ["1.SkullGreymon","2.MetalGreymon","3.Vademon","4.Sukamon"]
    val tyrannomon = ["1.Megadramon","2.MetalGreymon","3.Vademon","4.Sukamon"]
    val monochromon = ["1.MetalMamemon","2.MetalGreymon","3.Vademon","4.Sukamon"]
    val centarumon = ["1.Andromon","2.Giromon","3.Vademon","4.Sukamon"]
    val andromon = ["1.Sukamon"]
    val giromon = ["1.Sukamon"]
    val vademon = ["1.Sukamon"]
    val metalmamemon = ["1.Sukamon"]
    val garurumon = ["1.SkullGreymon","2.MegaSeadramon","3.Vademon","4.Sukamon"]
    val ogremon = ["1.Andromon","2.Giromon","3.Vademon","4.Sukamon"]
    val drimogemon = ["1.MetalGreymon","2.Vademon","3.Sukamon"]
    val angemon = ["1.Andromon","2.Devimon","3.Phoenixmon","4.Vademon","5.Sukamon"]
    val devimon = ["1.Megadramon","2.SullGreymon","3.Vademon","4.Sukamon"]
    val leomon = ["1.Andromon","2.Mamemon","3.Vademon","4.Sukamon"]
    val mamemon = ["1.Sukamon"]
    val kokatorimon = ["1.Piximon","2.Phoenixmon","3.Vademon","4.Sukamon"];
    val bakemon = ["1.Giromon","2.SkullGreymon","3.Vademon","4.Sukamon"]
    val mojyamon = ["1.SkullGreymon","2.Mamemon","3.Vademon","4.Sukamon"]
    val shellmon = ["1.HKabuterimon","2.MegaSeadramon","3.Vademon","4.Sukamon"]
    val coelamon = ["1.MegaSeadramon","2.Vademon","3.Sukamon"]
    val frigimon = ["1.MetalMamemon","2.Mamemon","3.Vademon","4.Sukamon"]
    val whamon = ["1.Mamemon","2.MegaSeadramon","3.Vademon","4.Sukamon"]
    val unimon = ["1.Phoenixmon","2.Giromon","3.Vademon","4.Sukamon"]
    val kabuterimon = ["1.HKabuterimon","2.MetalMamemon","3.Vademon","4.Sukamon"]
    val kuwagamon = ["1.Piximon","2.HKabuterimon","3.Vademon","4.Sukamon"]
    val vegiemon = ["1.Piximon","2.Vademon","3.Sukamon"]
    val ninjamon = ["1.MetalMamemon","2.Mamemon","3.Piximon","4.Vademon","5.Sukamon"]
    val seadramon = ["1.Megadramon","2.MegaSeadramon","3.Vademon","4.Sukamon"]
    val sukamon = ["1.Etemon","2.Vademon"]
    val numemon = ["1.Monzaemon","2.Vademon","3.Sukamon"]
    val nanimon = ["1.Digitamamon","2.Sukamon","3.Vademon"]
    val megaseadramon = ["1.Sukamon"]
    val hkabuterimon = ["1.Sukamon"]
    val piximon = ["1.Sukamon"]
    val etemon = ["1.Sukamon"]
    val monzaemon = ["1.Sukamon"]
    val digitamamon = ["1.Sukamon"]
    (*list of pairs (name - evolution) to turn into a table *)
    val evolist = [("Botamon",botamon), ("Punimon",punimon), ("Yuramon", yuramon), ("Poyomon",poyomon)
                  ,("Koromon",koromon), ("Tsunomon",tsunomon), ("Tanemon",tanemon), ("Tokomon",tokomon)
                  ,("Agumon",agumon), ("Gabumon",gabumon), ("Kunemon",kunemon), ("Patamon",patamon)
                  ,("Biyomon",biyomon), ("Elecmon",elecmon), ("Penguinmon",penguinmon), ("Palmon",palmon)
                  ,("Betamon",betamon), ("Birdramon",birdramon), ("Meramon",meramon), ("Greymon",greymon)
                  ,("Tyrannomon",tyrannomon), ("Monochromon",monochromon), ("Centarumon",centarumon)
                  ,("Drimogemon",drimogemon), ("Ogremon",ogremon), ("Garurumon",garurumon), ("Angemon",angemon)
                  ,("Leomon",leomon), ("Unimon",unimon), ("Kokatorimon",kokatorimon), ("Bakemon",bakemon)
                  ,("Airdramon",airdramon), ("Kabuterimon",kabuterimon), ("Vegiemon",vegiemon), ("Kuwagamon",kuwagamon)
                  ,("Whamon",whamon), ("Coelamon",coelamon), ("Ninjamon",ninjamon), ("Shellmon",shellmon)
                  ,("Seadramon",seadramon), ("Mojyamon",mojyamon), ("Frigimon",frigimon), ("Devimon",devimon)
                  ,("Numemon",numemon), ("Nanimon",nanimon), ("MetalGreymon",metalgreymon), ("SkullGreymon",skullgreymon)
                  ,("Metalmamemon",metalmamemon), ("Andromon",andromon), ("Giromon",giromon), ("Megadramon",megadramon)
                  ,("Phoenixmon",phoenixmon), ("HKabuterimon",hkabuterimon), ("Piximon",piximon), ("MegaSeadramon",megaseadramon)
                  ,("Mamemon",mamemon), ("Digitamamon",digitamamon), ("Monzaemon",monzaemon), ("Etemon",etemon)
                  ,("Vademon",vademon), ("Sukamon",sukamon)]
  in
    Table.fromList evolist
  end;

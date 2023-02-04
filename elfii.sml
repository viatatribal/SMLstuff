(* This is an Elf II emulator
 * It uses an RCA 1802 microprocessor
 * This version is the basic one, having 256 bytes of RAM
 * Hexadecimal keypad and two digital hexadecimal dispaly *)
 
 (* May need to fix overflow in add/sub operations *)
 
use "Word16.sml";
use "Word4.sml";

type byte = Word8.word;
type nibble = Word4.word4;
type word16 = Word16.word16;

datatype bit = Zero | One;

datatype register =
         D of byte
         | X of nibble
         | P of nibble
         | T of byte
         | DF of bit
         | IE of bit;

datatype instructions =
         IDL | SEQ | REQ | B1 | B2 | B3 | B4 | BN1 | BN2 | BN3 | BN4 |
         BR | LBR | OUT | INP | SKP | LSKP | NOP | BZ | BNZ | LBZ | LSZ |
         LSNZ | LBQ | LBNQ | BQ | BNQ | LSQ | LSNQ | INC of nibble | DEC of nibble |
         GLO of nibble | GHI of nibble | PLO of nibble | PHI of nibble | SEX of nibble | LDI |
         STR of nibble | STXD | LDA of nibble | LDN of nibble | IRX | LDX | LDXA | OR | AND |
         XOR | ORI | ANI | XRI | ADD | BDF | LBDF | BNF | LSDF | LSNF | ADI | ADC |
         ADCI | SD | SDI | SDBI | SDB | SM | SMI | SMB | SMBI | SHL | SHLC | SHR |
         SHRC | SEP of nibble | RET | DIS | LSIE | MARK | SAV | LBNZ | LBNF;

exception InstructionError;

type memory = byte vector;
type iButton = bit;
type qButton = bit;
(* RNs is a series of 16 registers going from R0-RF
 * one register of the series R is taken as the program counter *)
type RNs = word16 list;
type registers = register list;
(* may need to add some more stuff here *)
type CPU = memory * RNs * registers * qButton * iButton;

val ramsize = 256;

(* Utility Functions *)

(* word16 -> byte
 * Return low byte *)
fun lbword16 w : byte =
    let val lb = Word16.andb(w, Word16.fromInt 255)
    in
      Word8.fromInt(Word16.toInt lb)
    end;


(* word16 -> byte
 * Return high byte *)
fun hbword16 w : byte =
    let val sw = Word16.shiftr(w, 0wx8)
      val hb = Word16.andb(sw, Word16.fromInt 255)
    in
      Word8.fromInt(Word16.toInt hb)
    end;

(* byte -> nibble
 * Return low nibble *)
fun lnword8 w : nibble =
    let val lb = Word8.andb(w, Word8.fromInt 15)
    in
      Word4.fromInt(Word8.toInt lb)
    end;

(* byte -> nibble
 * Return high nibble *)
fun hnword8 w : nibble =
    let val sw = Word8.>>(w, 0wx4)
      val hb = Word8.andb(sw, Word8.fromInt 15)
    in
      Word4.fromInt(Word8.toInt hb)
    end;

(* bit -> bool
 * Check if Q is on *)
fun isQon Zero = false
  | isQon One  = true;

(* bit -> bool
 * Check if I is pressed *)
fun isIpressed Zero = false
  | isIpressed One  = true;

(* bit -> bool
 * Check if IE is on *)
fun isIEon (IE(Zero)) = false
  | isIEon (IE(One))  = true;

(* bit -> bool
 * Check if DF is on *)
fun isDFon (DF(Zero)) = false
  | isDFon (DF(One))  = true;

(* bit -> bit
 * Set I to  Zero or One *)
fun setI Zero = One
  | setI One = Zero;

(* bit -> int
 * Return 1 or 0 fron DF *)
fun getDF (DF(Zero)) = 0
  | getDF (DF(One))  = 1;

(* char -> string
 * Create a string representing the hex number *)
fun hexLetter n =
    case n of
        #"0" => "####\n#  #\n#  #\n#  #\n####\n"
      | #"1" => "#   \n#   \n#   \n#   \n#   \n"
      | #"2" => "####\n   #\n####\n#   \n####\n"
      | #"3" => "####\n   #\n####\n   #\n####\n"
      | #"4" => "#  #\n#  #\n####\n   #\n   #\n   #\n"
      | #"5" => "####\n#   \n####\n   #\n####\n"
      | #"6" => "####\n#   \n####\n#  #\n####\n"
      | #"7" => "####\n   #\n   #\n   #\n   #\n"
      | #"8" => "####\n#  #\n####\n#  #\n####\n"
      | #"9" => "####\n#  #\n####\n   #\n####\n"
      | #"A" => "####\n#  #\n####\n#  #\n#  #\n"
      | #"a" => "####\n#  #\n####\n#  #\n#  #\n"
      | #"B" => "#   \n#   \n####\n#  #\n####\n"
      | #"b" => "#   \n#   \n####\n#  #\n####\n"
      | #"C" => "####\n#   \n#   \n#   \n####\n"
      | #"c" => "####\n#   \n#   \n#   \n####\n"
      | #"D" => "   #\n   #\n####\n#  #\n####\n"
      | #"d" => "   #\n   #\n####\n#  #\n####\n"
      | #"E" => "####\n#   \n####\n#   \n####\n"
      | #"e" => "####\n#   \n####\n#   \n####\n"
      | #"F" => "####\n#   \n####\n#   \n#   \n"
      | #"f" => "####\n#   \n####\n#   \n#   \n"
      | _ => "";

(* string -> unit
 * Print a pair of hex numbers
 * It assumes that the string inputed ends with "\n"
 * and has length 3 => "XY\n", where X and Y are hex
 * numbers *)
fun printHexes str =
    let val [st1,st2,endl] = String.explode(str)
        val st1 = String.tokens (fn x => x = #"\n") (hexLetter st1)
        val st2 = String.tokens (fn x => x = #"\n") (hexLetter st2)
    in
      print (List.foldr (fn (x,b) => x ^ b) ""
                 (ListPair.map (fn (x,y) => x ^ "   " ^ y ^ "\n")
                               (st1, st2)))
    end;


(* char -> int
 * Take a hex letter and turns into an int *)
fun hexToInt n =
    case n of
        #"0" => 0
      | #"1" => 1
      | #"2" => 2
      | #"3" => 3
      | #"4" => 4
      | #"5" => 5
      | #"6" => 6
      | #"7" => 7
      | #"8" => 8
      | #"9" => 9
      | #"A" => 10
      | #"a" => 10
      | #"B" => 11
      | #"b" => 11
      | #"C" => 12
      | #"c" => 12
      | #"D" => 13
      | #"d" => 13
      | #"E" => 14
      | #"e" => 14
      | #"F" => 15
      | #"f" => 15;

(* string -> byte
 * Return a byte from the input string
 * It assumes that the string inputed ends with "\n"
 * and has length 3 => "XY\n", where X and Y are hex
 * numbers *)
fun strToByte str =
    let val [st1, st2, endl] = String.explode(str)
        val hex1 = (hexToInt st1) * 16
        val hex2 = hexToInt st2
    in
      Word8.fromInt (hex1 + hex2)
    end;

(* word -> word16
 * Convert word to word16 *)
val w16 = Word16.fromWord;

(* word -> byte
 * Convert word to byte *)
val w8 = Word8.fromLarge;

(* word -> nibble
 * Convert word to nibble *)
val w4 = Word4.fromWord;

(* memory -> unit
 * Output the values in the cpu's memory *)
fun printMemory mem =
    Vector.app (fn x => print ((Int.fmt StringCvt.HEX (Word8.toInt x))  ^ " "))
               mem;

(* byte -> instruction
 * A helper function to find the right nibble for some
 * instructions *)
fun dissNibble w =
    if (Word8.toLarge (Word8.andb(w, w8 0wxE0))) = 0wxE0
    then SEX(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wxB0))) = 0wxB0
    then PHI(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wxA0))) = 0wxA0
    then PLO(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx90))) = 0wx90
    then GHI(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx80))) = 0wx80
    then GLO(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx60))) = 0wx60
    then raise InstructionError
    else if (Word8.toLarge (Word8.andb(w, w8 0wx50))) = 0wx50
    then STR(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx40))) = 0wx40
    then LDA(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx20))) = 0wx20
    then DEC(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx10))) = 0wx10
    then INC(lnword8 w)
    else if (Word8.toLarge (Word8.andb(w, w8 0wx0F))) = (Word8.toLarge  w)
    then LDN(lnword8 w)
    else
      raise InstructionError;

(* byte -> instruction
 * Dissassembly a byte into a instruction *)
fun dissassembly w =
    case (Word8.toLarge w) of
        0wx74 => ADC
      | 0wx7C => ADCI
      | 0wxF4 => ADD
      | 0wxFC => ADI
      | 0wxF2 => AND
      | 0wxFA => ANI
      | 0wx34 => B1
      | 0wx35 => B2
      | 0wx36 => B3
      | 0wx37 => B4
      | 0wx33 => BDF
      | 0wx3C => BN1
      | 0wx3D => BN2
      | 0wx3E => BN3
      | 0wx3F => BN4
      | 0wx3B => BNF
      | 0wx39 => BNQ
      | 0wx3A => BNZ
      | 0wx31 => BQ
      | 0wx30 => BR
      | 0wx32 => BZ
      | 0wx71 => DIS
      | 0wx00 => IDL
      | 0wx60 => IRX
      | 0wxC3 => LBDF
      | 0wxCB => LBNF
      | 0wxC9 => LBNQ
      | 0wxCA => LBNZ
      | 0wxC1 => LBQ
      | 0wxC0 => LBR
      | 0wxC2 => LBZ
      | 0wxF8 => LDI
      | 0wxF0 => LDX
      | 0wx72 => LDXA
      | 0wxCF => LSDF
      | 0wxCC => LSIE
      | 0wxC8 => LSKP
      | 0wxC7 => LSNF
      | 0wxC5 => LSNQ
      | 0wxC6 => LSNZ
      | 0wxCD => LSQ
      | 0wxCE => LSZ
      | 0wx79 => MARK
      | 0wxC4 => NOP
      | 0wxF1 => OR
      | 0wxF9 => ORI
      | 0wx7A => REQ
      | 0wx70 => RET
      | 0wx78 => SAV
      | 0wxF5 => SD
      | 0wx75 => SDB
      | 0wx7D => SDBI
      | 0wxFD => SDI
      | 0wx7B => SEQ
      | 0wxFE => SHL
      | 0wx7E => SHLC
      | 0wxF6 => SHR
      | 0wx76 => SHRC
      | 0wx38 => SKP
      | 0wxF7 => SM
      | 0wx77 => SMB
      | 0wx7F => SMBI
      | 0wxFF => SMI
      | 0wx73 => STXD
      | 0wxF3 => XOR
      | 0wxFB => XRI
      | 0wx64 => OUT (* Only output to HexDisplay *)
      | 0wx6c => INP (* Only input from HexKeypad  *)
      | _ => dissNibble w;

(* nibble -> register
 * Return new value for register P *)
fun updateP w = P(w);

(* nibble -> register
 * Return new value for register X *)
fun updateX w = X(w);

(* register -> register
 * Copy P into X *)
fun copyPX (P(w)) = X(w);

(* byte * int -> register
 * Return new value for register D/T *)
fun updateDT (w,0) = D(w)
  | updateDT (w,1) = T(w);

(* register -> byte
 * Return the byte associated with D/T *)
fun getDT (D(w)) = w
  | getDT (T(w)) = w;

(* byte -> bit
 * Returns the most signficant bit *)
fun getMSB b =
    let val convertWord = (Word31.fromLargeWord o Word8.toLarge)
        val b' = Word8.>>(b, convertWord 0wx7)
    in
      if b' = Word8.fromInt 1
      then One
      else Zero
    end;

(* register -> nibble
 * Return the niggle associated with
 * the register *)
fun getNibble (X(n)) = n
  | getNibble (P(n)) = n;


(* register * Rregisters -> Rregister * int
 * Select one of the R registers pointed
 * by either register X or P *)
fun selectR re Rns =
    case re of
        P(x) => let val n = Word4.toInt x
                in (List.nth (Rns, n), n) end
      | X(x) => let val n = Word4.toInt x
                in (List.nth (Rns, n), n) end;

(* nibble * nibble -> byte
 * Convert two nibbles into a byte *)
fun nibblesToByte (w1, w2) =
    let val high = Word8.fromInt (Word4.toInt w1)
        val low  = Word8.fromInt (Word4.toInt w2)
        val high = Word8.<<(high, 0wx4)
    in Word8.+(high, low) end;


(* RN -> byte
 * Return the byte address associated
 * to a RN register
 * Since RAM is 256, we don't need to
 * fear losing extra bits *)
fun regToByte Rn =
    Word8.fromInt (Word16.toInt Rn);

(* RN * int -> byte
 * Increase RN by a number *)
fun increaseRN (Rn, i) =
    Word16.add(Rn, Word16.fromInt i);

(* byte -> word16
 * Update RN with a new byte *)
fun updateRN w =
    Word16.fromInt (Word8.toInt w);

(* byte * byte -> word16
 * Convert two bytes into a word16  *)
fun bytesToWord16 (w1, w2) =
    let val high = Word16.fromInt (Word8.toInt w1)
        val low  = Word16.fromInt (Word8.toInt w2)
        val high = Word16.shiftl(high, w16 0wx8)
    in Word16.add(high, low) end;

(* cpu * int -> Rn * int
 * Return register Rn and its position *)
fun getRN (cpu : CPU, pos) =
    let val px = List.nth (#3 cpu, pos)
        val (rn, posc) = selectR px (#2 cpu)
    in (rn, posc) end;

(* Register -> bool
 * Check if the accumulator is zero or not *)
fun isDZero (D(w)) =
    if (Word8.toInt w) = 0
    then true
    else false;

(* memory * RN -> byte
 * Gets the byte RN points to
 * in the memory *)
fun getMemoryAddress (memory, pc) =
    Vector.sub(memory, Word16.toInt pc);

(* RN * pos * memory -> byte * RN
 * Read memory, update PC
 * and return byte and PC *)
fun readMem (pc, pos, memory) =
    let val byte = getMemoryAddress (memory, pc)
        val pc' = increaseRN (pc, 1)
    in
      (byte, pc')
    end;

(* memory * byte * RNs * Registers -> memory * RNs
 * Update memory with new byte
 * and increase PC[the RN pointed by P] *)
fun updateMem (mem, w, Rns, Rs) =
    let val p = List.nth (Rs,1)
        val (pc, pos) = selectR p Rns
        val mem' = Vector.update (mem,Word8.toInt(regToByte pc),w)
        val pc' = increaseRN (pc, 1)
    in
      (mem', List.update (Rns, pos, pc'))
    end;

(* CPU -> CPU
 * Branch to the next two bytes *)
fun branchTwo (cpu : CPU) =
    let val (pc, pos) = getRN (cpu, 1)
        val (w1, pc')  = readMem (pc, pos, #1 cpu)
        val (w2, pc'') = readMem (pc', pos, #1 cpu)
        val  pc''' = bytesToWord16 (w1, w2)
    in
      (#1 cpu, List.update(#2 cpu, pos, pc'''), #3 cpu, #4 cpu, #5 cpu) : CPU
    end;


(* CPU -> CPU
 * Branch to the next byte *)
fun branchOne (cpu : CPU) =
    let val (pc, pos) = getRN (cpu, 1)
        val (w, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = updateRN w
    in
      (#1 cpu, List.update(#2 cpu, pos, pc''), #3 cpu, #4 cpu, #5 cpu) : CPU
    end;


(* CPU -> CPU
 * Skip the next two bytes *)
fun skipTwo (cpu : CPU) =
    let val (pc, pos) = getRN (cpu, 1)
        val pc' = increaseRN (pc, 2)
    in
      (#1 cpu, List.update(#2 cpu, pos, pc'), #3 cpu, #4 cpu, #5 cpu) : CPU
    end;


(* CPU -> CPU
 * Skip the next byte *)
fun skipOne (cpu : CPU) =
    let val (pc, pos) = getRN (cpu, 1)
        val pc' = increaseRN (pc, 1)
    in
      (#1 cpu, List.update(#2 cpu, pos, pc'), #3 cpu, #4 cpu, #5 cpu) : CPU
    end;

(* CPU * instruction -> CPU
 * Do logical operations on D *)
fun logicalOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 2)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           OR  => let val w1 = getDT(List.hd (#3 cpu))
                      val d = updateDT(Word8.orb(w1,w2),0)
                  in (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(#3 cpu,0,d),#4 cpu, #5 cpu) end
         | AND => let val w1 = getDT(List.hd (#3 cpu))
                      val d = updateDT(Word8.andb(w1,w2),0)
                  in (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(#3 cpu,0,d),#4 cpu, #5 cpu) end
         | XOR => let val w1 = getDT(List.hd (#3 cpu))
                      val d = updateDT(Word8.xorb(w1,w2),0)
                  in (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(#3 cpu,0,d),#4 cpu, #5 cpu) end
      )
    end;

(* CPU * instruction -> CPU
 * Do immediate logical operations on D *)
fun logicalIOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 1)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           ORI  => let val w1 = getDT(List.hd (#3 cpu))
                      val d = updateDT(Word8.orb(w1,w2),0)
                  in (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(#3 cpu,0,d),#4 cpu, #5 cpu) end
         | ANI => let val w1 = getDT(List.hd (#3 cpu))
                      val d = updateDT(Word8.andb(w1,w2),0)
                  in (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(#3 cpu,0,d),#4 cpu, #5 cpu) end
         | XRI => let val w1 = getDT(List.hd (#3 cpu))
                      val d = updateDT(Word8.xorb(w1,w2),0)
                  in (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(#3 cpu,0,d),#4 cpu, #5 cpu) end
      )
    end;

(* CPU * instruction -> CPU
 * Do addition operations on D *)
fun addOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 2)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           ADD  => let val w1 = getDT(List.hd (#3 cpu))
                       val d = updateDT(Word8.+(w1,w2),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end
         | ADC  => let val w1 = getDT(List.hd (#3 cpu))
                       val df = getDF(List.nth (#3 cpu, 3))
                       val d = updateDT(Word8.+(Word8.+(w1,w2), Word8.fromInt df),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end)
    end;

(* CPU * instruction -> CPU
 * Do immediate addition operations on D *)
fun addIOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 1)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           ADI   => let val w1 = getDT(List.hd (#3 cpu))
                       val d = updateDT(Word8.+(w1,w2),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                    end
         | ADCI  => let val w1 = getDT(List.hd (#3 cpu))
                       val df = getDF(List.nth (#3 cpu,3))
                       val d = updateDT(Word8.+(Word8.+(w1,w2), Word8.fromInt df),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end)
    end;


(* CPU * instruction -> CPU
 * Do subtraction operations on D *)
fun subDOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 2)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           SD   => let val w1 = getDT(List.hd (#3 cpu))
                       val d = updateDT(Word8.+(w2,Word8.~(w1)),0)
                   in if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end
         | SDB  => let val w1 = getDT(List.hd (#3 cpu))
                       val df = getDF(List.nth (#3 cpu,3))
                       val d = updateDT(Word8.+(Word8.+(w2,Word8.~(w1)), Word8.fromInt df),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end)
    end;

(* CPU * instruction -> CPU
 * Do immediate subtraction operations on D *)
fun subIDOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 1)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           SDI  => let val w1 = getDT(List.hd (#3 cpu))
                       val d = updateDT(Word8.+(w2,Word8.~(w1)),0)
                   in  if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end
         | SDBI => let val w1 = getDT(List.hd (#3 cpu))
                       val df = getDF(List.nth (#3 cpu,3))
                       val d = updateDT(Word8.+(Word8.+(w2,Word8.~(w1)), Word8.fromInt df),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end)
    end;

(* CPU * instruction -> CPU
 * Do subtraction operations from D *)
fun subOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 2)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           SM   => let val w1 = getDT(List.hd (#3 cpu))
                       val d = updateDT(Word8.+(w1,Word8.~(w2)),0)
                   in  if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end
         | SMB  => let val w1 = getDT(List.hd (#3 cpu))
                       val df = getDF(List.nth (#3 cpu,3))
                       val d = updateDT(Word8.+(Word8.+(w1,Word8.~(w2)), Word8.fromInt df),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end)
    end;

(* CPU * instruction -> CPU
 * Do immediate subtraction operations from D *)
fun subIOP (cpu : CPU, inst) : CPU =
    let val (pc, pos) = getRN (cpu, 1)
        val (w2, pc') = readMem (pc, pos, #1 cpu)
        val pc'' = increaseRN (pc', 1)
    in
      (case inst of
           SMI  => let val w1 = getDT(List.hd (#3 cpu))
                       val d = updateDT(Word8.+(w1,Word8.~(w2)),0)
                   in  if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end
         | SMBI => let val w1 = getDT(List.hd (#3 cpu))
                       val df = getDF(List.nth (#3 cpu,3))
                       val d = updateDT(Word8.+(Word8.+(w1,Word8.~(w2)), Word8.fromInt df),0)
                   in
                     if w1 > getDT(d) then
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(One)),#4 cpu, #5 cpu)
                     else
                       (#1 cpu, List.update(#2 cpu,pos,pc''), List.update(List.update(#3 cpu,0,d),3,DF(Zero)),#4 cpu, #5 cpu)
                   end)
    end;

(* CPU * instruction -> CPU
 * Do shift operations from D *)
fun shiftOP (cpu : CPU, inst) : CPU =
    let val d = getDT(List.hd (#3 cpu))
        val b = getMSB d
        val df = getDF(List.nth (#3 cpu, 3))
        val z8 = w8 0wx0
        val convertWord = (Word31.fromLargeWord o Word8.toLarge)
    in
      (case inst of
           SHL  => let val d' = Word8.<<(d, convertWord 0wx1)
                   in
                     (#1 cpu, #2 cpu, List.update(List.update(#3 cpu,0,updateDT(d',0)),3,DF(b)), #4 cpu, #5 cpu)
                   end
         | SHLC => let val d' = Word8.<<(d, convertWord 0wx1)
                       val d'' = Word8.+(d', Word8.fromInt df)
                   in
                     (#1 cpu, #2 cpu, List.update(List.update(#3 cpu,0,updateDT(d'',0)),3,DF(b)), #4 cpu, #5 cpu)
                   end
         | SHR  => let val d' = Word8.>>(d, convertWord 0wx1)
                   in
                     (#1 cpu, #2 cpu, List.update(List.update(#3 cpu,0,updateDT(d',0)),3,DF(b)), #4 cpu, #5 cpu)
                   end
         | SHRC => let val d' = Word8.>>(d, convertWord 0wx1)
                       val df' = Word8.<<(Word8.fromInt df, convertWord 0wx7)
                       val d'' = Word8.+(d',df')
                   in
                     (#1 cpu, #2 cpu, List.update(List.update(#3 cpu,0,updateDT(d'',0)),3,DF(b)), #4 cpu, #5 cpu)
                   end)
    end;


(* Main Functions *)

(* unit -> CPU
 * Start the initial values of the cpu *)
fun cpuInit ()  =
    let val z16 = w16 0wx0
        val z8 = w8 0wx0
        val z4 = w4 0wx0
        val Rns = List.tabulate (16, fn x => z16) : RNs
        val Rs = [D(z8),P(z4),X(z4),
                  DF(Zero),IE(Zero),T(z8)] : registers
        val mem = Vector.tabulate (ramsize, fn x => Word8.fromInt 0) : memory
        val qButton = Zero : qButton
        val iButton = Zero : iButton
    in
      (mem, Rns, Rs, qButton, iButton) : CPU
    end;

(* CPU -> CPU
 * Reset CPU *)
fun cpuReset (cpu : CPU) =
    let val z16 = w16 0wx0
        val z4 = 0wx0
        val Rns' = z16 :: (List.tl (#2 cpu))
        val [d,p,x,df,ie,t] = #3 cpu
        val ie' = IE(One)
        val x' = X(z4)
        val p' = P(z4)
    in
      (#1 cpu, Rns', [d,p',x',df,ie',t], #4 cpu, #5 cpu) : CPU
    end;

(* CPU * string -> CPU
 * Load the input into memory
 * and also prints the input read *)
fun cpuLoad (cpu : CPU, str) =
    let val byte = strToByte str
        val (mem, Rns) = updateMem (#1 cpu, byte, #2 cpu, #3 cpu)
    in
      ( printHexes str
       ;(mem, Rns, #3 cpu, #4 cpu, #5 cpu) : CPU
      )
    end;

(* CPU -> CPU
 * Get input from user, store in memory
 * and in the accumulator *)
fun cpuInput (cpu : CPU) =
    let val str = valOf (TextIO.inputLine(TextIO.stdIn))
        val w = strToByte str
        val (rx, pos) = getRN (cpu, 2)
        val mem = Vector.update(#1 cpu, Word16.toInt rx, w)
        val d = updateDT(w, 0)
        val (rc, posrc) = getRN (cpu, 1)
        val rc' = increaseRN(rc,1)
    in
      ( printHexes str
       ;(mem,List.update(#2 cpu, posrc, rc'), List.update(#3 cpu, 0, d), #4 cpu, #5 cpu) : CPU
      )
    end;

(* CPU -> CPU
 * Output to user and increment RX *)
fun cpuOutput (cpu : CPU) =
    let val (rx, pos) = getRN (cpu, 2)
        val (rc, posrc) = getRN (cpu, 1)
        val w = Vector.sub(#1 cpu, Word16.toInt rx)
        val str = (Int.fmt StringCvt.HEX (Word8.toInt w)) ^ "\n"
        val rx' = increaseRN(rx,1)
        val rc' = increaseRN(rc,1)
        val cpu' = (#1 cpu, List.update(List.update(#2 cpu, pos, rx'),posrc,rc'), #3 cpu, #4 cpu, #5 cpu) : CPU
    in
      if String.size str < 3
      then
        ( printHexes ("0" ^ str)
        ; cpu'
        )
      else
        ( printHexes str
        ; cpu'
        )
    end;

(* CPU * instruction -> CPU
 * Decode an instruction *)
fun cpuDecode (cpu : CPU, instruction) =
    case instruction of
        (* Idle *)
        IDL    => cpu
      (* Set Q *)
      | SEQ    => let val qButton = One
                  in (#1 cpu, #2 cpu, #3 cpu, qButton, #5 cpu) end
      (* Reset Q *)
      | REQ    => let val qButton = Zero
                  in (#1 cpu, #2 cpu, #3 cpu, qButton, #5 cpu) end
      (* B1 always skip in Elf II *)
      | B1     => skipOne cpu
      (* B2 always skip in Elf II *)
      | B2     => skipOne cpu
      (* B3 always skip in Elf II *)
      | B3     => skipOne cpu
      (* Branch if I is pressed *)
      | B4     => if isIpressed (#5 cpu)
                  then branchOne cpu
                  else skipOne cpu
      (* BN1 always branch in Elf II *)
      | BN1    => branchOne cpu
      (* BN2 always branch in Elf II *)
      | BN2    => branchOne cpu
      (* BN3 always branch in Elf II *)
      | BN3    => branchOne cpu
      (* Branch if I is not pressed *)
      | BN4    => if isIpressed (#5 cpu)
                  then skipOne cpu
                  else branchOne cpu
      (* Always branch *)
      | BR     => branchOne cpu
      (* Always branch *)
      | LBR    => branchTwo cpu
      (* Output from memory *)
      | OUT => cpuOutput cpu
      (* Input to memory and also store it in D *)
      | INP => cpuInput cpu
      (* Skip the next byte *)
      | SKP    => skipOne cpu
      (* Skip the next two bytes *)
      | LSKP   => skipTwo cpu
      (* Do nothing *)
      | NOP    => cpu
      (* Branch if the accumulator is zero *)
      | BZ     => if isDZero(List.hd (#3 cpu))
                  then branchOne cpu
                  else skipOne cpu
      (* Branch if the accumulator is not zero *)
      | BNZ    => if isDZero(List.hd (#3 cpu))
                  then skipOne cpu
                  else branchOne cpu
      (* Branch if the accumulator is zero *)
      | LBZ    => if isDZero(List.hd (#3 cpu))
                  then branchTwo cpu
                  else skipTwo cpu
      (* Branch if the accumulator is not zero *)
      | LBNZ   => if isDZero(List.hd (#3 cpu))
                  then skipTwo cpu
                  else branchTwo cpu
      (* Skip two bytes if the accumulator is zero *)
      | LSZ    => if isDZero(List.hd (#3 cpu))
                  then skipTwo cpu
                  else cpu
      (* Skip two bytes if the accumulator is not zero *)
      | LSNZ   => if isDZero(List.hd (#3 cpu))
                  then cpu
                  else skipTwo cpu
      (* Branch if Q is on *)
      | LBQ    => if isQon(#4 cpu)
                  then branchTwo cpu
                  else skipTwo cpu
      (* Branch if Q is off *)
      | LBNQ   => if isQon(#4 cpu)
                  then skipTwo cpu
                  else branchTwo cpu
      (* Branch if Q is on *)
      | BQ     => if isQon(#4 cpu)
                  then branchOne cpu
                  else skipOne cpu
      (* Branch if Q is off *)
      | BNQ    => if isQon(#4 cpu)
                  then skipOne cpu
                  else branchOne cpu
      (* Skip two bytes if Q is on *)
      | LSQ    => if isQon(#4 cpu)
                  then skipTwo cpu
                  else cpu
      (* Skip two bytes if Q is off *)
      | LSNQ   => if isQon(#4 cpu)
                  then cpu
                  else skipTwo cpu
      (* Increment register Rr *)
      | INC(r) => let val pos = Word4.toInt r
                      val rn = List.nth (#2 cpu, pos)
                      val rn' = increaseRN(rn,1)
                  in (#1 cpu, List.update(#2 cpu, pos, rn'), #3 cpu, #4 cpu, #5 cpu)  end
      (* Decrement register Rr *)
      | DEC(r) => let val pos = Word4.toInt r
                      val rn = List.nth (#2 cpu, pos)
                      val rn' = Word16.sub(rn, w16 0wx1)
                  in (#1 cpu, List.update(#2 cpu, pos, rn'), #3 cpu, #4 cpu, #5 cpu) end
      (* Load low byte of register Rr into accumulator *)
      | GLO(r) => let val pos = Word4.toInt r
                      val rn = List.nth (#2 cpu, pos)
                      val d = updateDT(lbword16(rn),0)
                  in (#1 cpu, #2 cpu, List.update(#3 cpu, 0,d), #4 cpu, #5 cpu) end
      (* Load high byte of register Rr into accumulator *)
      | GHI(r) => let val pos = Word4.toInt r
                      val rn = List.nth (#2 cpu, pos)
                      val d = updateDT(hbword16(rn),0)
                  in (#1 cpu, #2 cpu, List.update(#3 cpu, 0,d), #4 cpu, #5 cpu) end
      (* Store D into low byte of register Rr *)
      | PLO(r) => let val pos = Word4.toInt r
                      val d = List.nth (#3 cpu, 0)
                      val rn = Word16.fromInt (Word8.toInt (getDT d))
                  in (#1 cpu, List.update(#2 cpu, pos, rn), #3 cpu, #4 cpu, #5 cpu) end
      (* Store D into high byte of register Rr *)
      | PHI(r) => let val pos = Word4.toInt r
                      val d = List.nth (#3 cpu, 0)
                      val rn = Word16.shiftl(Word16.fromInt (Word8.toInt (getDT d)),w16 0wx8);
                  in (#1 cpu, List.update(#2 cpu, pos, rn), #3 cpu, #4 cpu, #5 cpu) end
      | SEX(r) => (#1 cpu, #2 cpu, List.update(#3 cpu, 2, X(r)), #4 cpu, #5 cpu) (* Set X *)
      (* Load byte into D *)
      | LDI    => let val (pc, pos) = getRN (cpu, 1)
                      val (w, pc')  = readMem (pc, pos, #1 cpu)
                      val d = updateDT(w,0)
                  in (#1 cpu, List.update(#2 cpu, pos, pc'), List.update(#3 cpu, 0, d), #4 cpu, #5 cpu) end
      (* Store D into the memory R(r) points to *)
      | STR(r) => let val pos = Word4.toInt r
                      val rn = List.nth (#2 cpu, pos)
                      val w = Word4.toInt (getNibble (List.nth (#3 cpu, 2)))
                      val mempos = Word8.toInt (regToByte rn)
                      val mem = Vector.update(#1 cpu, mempos, Word8.fromInt w)
                  in (mem, #2 cpu, #3 cpu, #4 cpu, #5 cpu) end
      (* Store D into R(X) and decrease R(X) *)
      | STXD   => let val (rx, pos) = getRN (cpu, 2)
                      val w = Word4.toInt (getNibble (List.nth (#3 cpu, 2)))
                      val mempos = Word8.toInt (regToByte rx)
                      val mem = Vector.update(#1 cpu, mempos, Word8.fromInt w)
                      val rx' = Word16.sub(rx, w16 0wx1)
                  in (mem, List.update(#2 cpu, pos, rx'), #3 cpu, #4 cpu, #5 cpu) end
      (* Load D from R(r) and increase R(r) *)
      | LDA(r) => let val pos = Word4.toInt r
                      val rn = List.nth (#2 cpu, pos)
                      val w = Vector.sub(#1 cpu, pos)
                      val d = updateDT(w, 0)
                      val rn' = increaseRN(rn, 1)
                  in (#1 cpu, List.update(#2 cpu, pos, rn'), List.update(#3 cpu, 0, d), #4 cpu, #5 cpu) end
      (* Load D from R(r) *)
      | LDN(r) => let val pos = Word4.toInt r
                      val w = Vector.sub(#1 cpu, pos)
                      val d = updateDT(w, 0)
                  in (#1 cpu, #2 cpu, List.update(#3 cpu, 0, d), #4 cpu, #5 cpu) end
      (* Increment R(X) *)
      | IRX    => let val (rn, pos) = getRN (cpu, 2)
                      val rx = Word16.add(rn, w16 0wx1)
                  in (#1 cpu, List.update(#2 cpu, pos, rx), #3 cpu, #4 cpu, #5 cpu) end
      (* Load D via R(X) *)
      | LDX    => let val (rn, pos) = getRN (cpu, 2)
                      val (w, pc) = readMem (rn, pos, #1 cpu)
                      val d = updateDT(w,0)
                  in (#1 cpu, #2 cpu, List.update(#3 cpu, 0, d), #4 cpu, #5 cpu) end
      (* Load D via R(X) and increment R(X)*)
      | LDXA   => let val (rn, pos) = getRN (cpu, 2)
                      val (w, rx) = readMem (rn, pos, #1 cpu)
                      val d = updateDT(w,0)
                  in (#1 cpu, List.update(#2 cpu, pos, rx), List.update(#3 cpu, 0, d), #4 cpu, #5 cpu) end
      (* Logical OR operation on D, store result in D *)
      | OR     => logicalOP(cpu, OR)
      (* Logical AND operation on D, store result in D *)
      | AND    => logicalOP(cpu, AND)
      (* Logical XOR operation on D, store result in D *)
      | XOR    => logicalOP(cpu, XOR)
      (* Logical OR immediate operation on D, store result in D *)
      | ORI    => logicalOP(cpu, ORI)
      (* Logical AND immediate operation on D, store result in D *)
      | ANI    => logicalOP(cpu, ANI)
      (* Logical XOR immediate operation on D, store result in D *)
      | XRI    => logicalOP(cpu, XRI)
      (* Branch one byte if DF is 1 *)
      | BDF    => if isDFon (List.nth(#3 cpu, 3))
                  then branchOne cpu
                  else skipOne cpu
      (* Branch two bytes if DF is 1 *)
      | LBDF   => if isDFon (List.nth(#3 cpu, 3))
                  then branchTwo cpu
                  else skipTwo cpu
      (* Branch one byte if DF is 0 *)
      | BNF    => if isDFon (List.nth(#3 cpu, 3))
                  then skipOne cpu
                  else branchOne cpu
      (* Branch two bytes if DF is 0 *)
      | LBNF   => if isDFon (List.nth(#3 cpu, 3))
                  then skipTwo cpu
                  else branchTwo cpu
      (* Skip two bytes if DF is 1 *)
      | LSDF   => if isDFon (List.nth(#3 cpu, 3))
                  then skipTwo cpu
                  else cpu
      (* Skip two bytes if DF is 0 *)
      | LSNF   => if isDFon (List.nth(#3 cpu, 3))
                  then cpu
                  else skipTwo cpu
      (* Add R(X) on D, store result in D *)
      | ADD    => addOP(cpu,ADD)
      (* Add immediate byte on D, store result in D *)
      | ADI    => addIOP(cpu,ADI)
      (* Add with carry R(X) on D, store result in D *)
      | ADC    => addOP(cpu,ADC)
      (* Add immediate byte with carry on D, store result in D *)
      | ADCI   => addIOP(cpu,ADCI)
      (* Subtract D from R(X), store result in D *)
      | SD     => subDOP(cpu,SD)
      (* Subtract D from immediate byte, store result in D *)
      | SDI    => subIDOP(cpu,SDI)
      (* Subtract D with borrow from immediate byte, store result in D *)
      | SDBI   => subIDOP(cpu,SDBI)
      (* Subtract D with borrow from R(X), store result in D *)
      | SDB    => subDOP(cpu,SDB)
      (* Subtract R(X) from D, store result in D *)
      | SM     => subOP(cpu,SM)
      (* Subtract immediate byte from D, store result in D *)
      | SMI    => subIOP(cpu,SMI)
      (* Subtract R(X) with borrow from D, store result in D *)
      | SMB    => subOP(cpu,SMB)
      (* Subtract immediate byte with borrow from D, store result in D *)
      | SMBI   => subIOP(cpu,SMBI)
      (*Shift D left *)
      | SHL    => shiftOP(cpu,SHL)
      (*Shift D left with carry *)
      | SHLC   => shiftOP(cpu,SHLC)
      (*Shift D right *)
      | SHR    => shiftOP(cpu,SHR)
      (*Shift D right with carry *)
      | SHRC   => shiftOP(cpu,SHRC)
      | SEP(r) => let val pos = Word4.toInt r (* Set P *)
                      val rn = List.nth (#2 cpu, pos)
                      val w = lbword16 rn
                      val p = updateP(w)
                  in (#1 cpu, #2 cpu, List.update(#3 cpu, 1, p), #4 cpu, #5 cpu) end
      (* Return from the interrupt *)
      | RET    => let val (rn, pos) = getRN (cpu, 2)
                      val (w, rn') = readMem (rn, pos, #1 cpu)
                      val p = updateP(lnword8 w)
                      val x = updateX(hnword8 w)
                      val ie = IE(One)
                      val rs = List.update(List.update(List.update(#3 cpu,1,p),2,x),4,ie)
                  in (#1 cpu, List.update(#2 cpu, pos, rn'), rs, #4 cpu, #5 cpu) end
      (* Return and disable interrupts *)
      | DIS    => let val (rn, pos) = getRN (cpu, 2)
                      val (w, rn') = readMem (rn, pos, #1 cpu)
                      val p = updateP(lnword8 w)
                      val x = updateX(hnword8 w)
                      val ie = IE(Zero)
                      val rs = List.update(List.update(List.update(#3 cpu,1,p),2,x),4,ie)
                  in (#1 cpu, List.update(#2 cpu, pos, rn'), rs, #4 cpu, #5 cpu) end
      (* Long Skip if Interrupts are On *)
      | LSIE   => if isIEon (List.nth (#3 cpu, 6))
                  then skipTwo cpu
                  else cpu
      (* Save X and P in T *)
      | MARK   => let val w1 = getNibble(List.nth(#3 cpu, 2))
                      val w2 = getNibble(List.nth(#3 cpu, 2))
                      val t = updateDT(nibblesToByte(w1, w2), 1)
                      val r2 = List.nth(#2 cpu, 2)
                      val mem = Vector.update(#1 cpu,Word16.toInt(r2),getDT t)
                      val r2' = Word16.sub(r2, w16 0wx1)
                      val x = copyPX(List.nth(#3 cpu, 1))
                      val rs = List.update(List.update(#3 cpu, 2, x),5,t)
                  in (mem, List.update(#2 cpu, 2, r2'), rs, #4 cpu, #5 cpu) end
      (* Save T *)
      | SAV    => let val (rn, pos) = getRN (cpu, 2)
                      val t = List.nth(#3 cpu, 5)
                      val mem = Vector.update(#1 cpu,Word16.toInt(rn),getDT t)
                  in (mem, #2 cpu, #3 cpu, #4 cpu, #5 cpu) end;


(* CPU -> CPU * instruction
 * Read memory from CPU, update PC
 * and get a new instruction *)
fun cpuRead (cpu : CPU) =
    let val (pc, pos) = getRN (cpu, 1)
        val byte = getMemoryAddress (#1 cpu, pc)
        val pc' = increaseRN (pc, 1)
        val inst = dissassembly byte
    in
      ((#1 cpu, List.update(#2 cpu, pos, pc'), #3 cpu, #4 cpu, #5 cpu) : CPU,
       inst)
    end;

(* CPU -> CPU
 * Run the cpu *)
fun cpuRun (cpu : CPU) =
    let val cpu' = cpuReset cpu
        fun run ncpu n = if n >= ramsize then ncpu
                    else
                     let val (ncpu', inst) = cpuRead ncpu
                         val newcpu = cpuDecode (ncpu', inst)
                     in
                       if inst = IDL then newcpu
                       else
                         run newcpu (n + 1) end
    in run cpu' 0 end;

(* unit -> unit
 * Main function of the program *)
fun cpuLoop () =
    let val cpu = cpuInit()
        (* for Q output *)
        fun QOutput q =
            if isQon q then "Q is on\n"
            else "Q is off\n";
        (* for I output *)
        fun IOutput i =
            if isIpressed i then "I is pressed\n"
            else "I is not pressed\n";
        (* Check for valid input to store in memory *)
        fun validInput str =
            let val valid = String.explode "0123456789AaBbCcDdEeFf\n"
            in
              List.all (fn x => (List.exists (fn y => x = y) valid))
                       (String.explode str)
            end;
        (* The main output dialogue *)
        fun elfiiInput (q, i) =
            ( TextIO.output(TextIO.stdOut, QOutput q)
            ; TextIO.output(TextIO.stdOut, IOutput i)
            ; TextIO.output(TextIO.stdOut, "Elf II: ")
            ; TextIO.flushOut(TextIO.stdOut)
            ; valOf (TextIO.inputLine(TextIO.stdIn))
            );
        (* To output cpu's memory *)
        fun memory (cpu : CPU) =
            (printMemory (#1 cpu)
            ; TextIO.output(TextIO.stdOut, "\n")
            ; TextIO.flushOut(TextIO.stdOut)
            ; readInput (elfiiInput(#4 cpu, #5 cpu)) cpu )
        (* The main loop of the cpu *)
        and readInput input (cpu : CPU) =
            if input = "Run\n"
            then let val cpu' = cpuRun cpu
                 in readInput (elfiiInput(#4 cpu', #5 cpu')) cpu' end
            else if input = "I\n"
            then let val cpu' = (#1 cpu, #2 cpu, #3 cpu, #4 cpu, setI (#5 cpu))
                 in readInput (elfiiInput(#4 cpu', #5 cpu')) cpu' end
            else if input = "Reset\n"
            then let val cpu' = cpuReset cpu
                 in readInput (elfiiInput(#4 cpu', #5 cpu')) cpu' end
            else if input = "Memory\n"
            then memory cpu
            else if input = "Exit\n"
            then ()
            else if validInput input
            then let val cpu' = cpuLoad (cpu, input)
                 in readInput (elfiiInput(#4 cpu', #5 cpu')) cpu' end
            else readInput (elfiiInput(#4 cpu, #5 cpu)) cpu;
    in
      readInput (elfiiInput (#4 cpu, #5 cpu)) cpu
    end;

(* A Pretty Printer, based on Philip Wadler's "A prettier printer".
   But heavily modified to be efficient in a strict language.
   http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html

   Copyright 1997, 1998, 1999, 2000, 2001 Ken Friis Larsen <ken@friislarsen.net>

   This code is released under GNU LGPL version 2 or any later after
   your choice, the licence can be obtained at
   http://www.gnu.org/copyleft/lgpl.html
*)

functor PrettyPrint (C : ANSI_COLORS) :> PRETTY_PRINT =
struct
  infixr 6 ^^

  structure C = C

  type fmt =
    {color : C.color,
     bold : bool,
     ul : bool,
     bl : bool}

  datatype doc =
     NIL
   | APPEND of doc * doc
   | NEST of int * doc
   | TEXT of string
   | BREAK of int * int
   | NEWLINE
   | GROUP of doc
   | FORMAT of (fmt -> fmt) * doc
   | RULE of char

  val op^^ =
    fn (NIL, NIL) => NIL
     | (NIL, y) => y
     | (x, NIL) => x
     | p => APPEND p

  val empty = NIL
  fun nest i x = NEST(i, x)
  val text = TEXT
  fun break sp off = BREAK (sp, off)
  val line = BREAK (1, 0)
  val newline = NEWLINE
  val rule = RULE
  fun group x = GROUP x

  fun color cl x = FORMAT (fn {color, bold, ul, bl} => {color = cl, bold = bold, ul = ul, bl = bl}, x)
  fun bold b x = FORMAT (fn {color, bold, ul, bl} => {color = color, bold = b, ul = ul, bl = bl}, x)
  fun underline b x = FORMAT (fn {color, bold, ul, bl} => {color = color, bold = bold, ul = b, bl = bl}, x)
  fun blink b x = FORMAT (fn {color, bold, ul, bl} => {color = color, bold = bold, ul = ul, bl = b}, x)

  (*** Derived functions ***)
  val concat = List.foldr op^^ empty

  fun seq sep ppr xs =
    let
      fun iter nil acc = acc
        | iter [x] acc = acc ^^ ppr x
        | iter (x::xs) acc = iter xs (acc ^^ ppr x ^^ sep)
    in
      iter xs empty
    end

  fun fromConv conv = text o conv

  val int = fromConv Int.toString
  val char = fromConv Char.toString
  val word = fromConv Word.toString
  val real = fromConv Real.toString
  fun bool b = if b then text "true" else text "false"


  (*** Formating of docs ***)

  val nlsize = String.size "\n"

  fun spaces outs s i =
    outs s (StringCvt.padLeft #" " i "")

  fun nlspace outs s i =
    outs s (StringCvt.padRight #" " (i+nlsize) "\n")


  fun renderFmt ansi {color, bold, ul, bl} =
    if ansi then
      C.color color
        ^ C.bold bold
        ^ C.underline ul
        ^ C.blink bl
    else
      ""

  local
    datatype mode = Flat | Break

    fun fitting [] left = true
      | fitting ((i, mode, fmt, doc) :: rest) left =
        if left >= 0 then
          case doc of
             NIL => fitting rest left
           | APPEND (x, y) => fitting ((i, mode, fmt, x) :: (i, mode, fmt, y) :: rest) left
           | NEST (j, x) => fitting ((i + j, mode, fmt, x) :: rest) left
           | TEXT s => fitting rest (left - String.size s)
           | BREAK (sp, _) =>
             (case mode of
                 Flat  => fitting rest (left - sp)
               | Break => true)
           | NEWLINE => true
           | RULE _ => true
           | GROUP x => fitting ((i, mode, fmt, x) :: rest) left
           | FORMAT (f, doc') => fitting ((i, mode, f fmt, doc') :: rest) left
        else
          false
  in

  (* w    : linewidth
     outs : function to output a string
     s    : state for outs
     k    : number of chars already used on current line
     i    : indent after linebreaks
  *)
    fun best (w : int) ansi (outs : 'a -> string -> 'a) (s : 'a) (x : doc) : 'a =
      let
        fun be s k [] = if ansi then outs s C.reset else s
          | be s k ((i, mode, fmt, doc) :: rest) =
            (case doc of
                NIL => be s k rest
              | APPEND (x, y) => be s k ((i, mode, fmt, x) :: (i, mode, fmt, y) :: rest)
              | NEST (j, x) => be s k ((i + j, mode, fmt, x) :: rest)
              | TEXT str => let val s = outs s (renderFmt ansi fmt ^ str) in be s (k + String.size str) rest end
              | NEWLINE => let val s = nlspace outs s i in be s i rest end
              | BREAK (sp, off) =>
                (case mode of
                    Flat => let val s = spaces outs s sp in be s (k + sp) rest end
                  | Break => let val s = nlspace outs s (i + off) in be s (i + off) rest end)
              | GROUP x =>
                (case mode of
                    Flat => be s k ((i, Flat, fmt, x) :: rest)
                  | Break =>
                      let
                        val mode' = if fitting ((i, Flat, fmt, x) :: rest) (w - k) then Flat else Break
                      in
                        be s k ((i, mode', fmt, x) :: rest)
                      end)
              | RULE c =>
                  let
                    val str = String.implode (List.tabulate (w - k, fn _ => c))
                  in
                    be s k ((i, mode, fmt, TEXT str) :: (i, mode, fmt, NEWLINE) :: rest)
                  end
              | FORMAT (f, x) =>
                  be s k ((i, mode, f fmt, x) :: rest))
      in
        be s 0 [(0, Break, {color = C.black, bold = false, ul = false, bl = false}, x)]
      end
  end

  fun toOutStream w outstream doc =
    let
      fun outs () s = TextIO.output(outstream, s)
    in
      best w false outs () doc;
      outs () "\n";
      TextIO.flushOut outstream
    end

  fun toFile w filename doc =
    let
      val dev = TextIO.openOut filename
    in
      toOutStream w dev doc handle ? => (TextIO.closeOut dev; raise ?);
      TextIO.closeOut dev
    end

  fun toString w ansi doc =
    let
      fun outs strs s = s :: strs
      val strs = best w ansi outs [] doc
    in
      String.concat (List.rev ("\n" :: strs))
    end

  val toConsumer = fn w => best w false
end

structure PrettyPrint = PrettyPrint (AnsiColors)

(* Copyright (c) 2011-2015 Michael J. Sullivan
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * Original location: https://github.com/msullivan/sml-util
 *)

structure AnsiColors :> ANSI_COLORS =
struct
  (* Control Sequence Introducer *)
  val CSI = "\^[["

  (* Send a one argument command *)
  fun cmd1 s n = CSI ^ (Int.toString n) ^ s
  (* Send a two argument command *)
  fun cmd2 s (n, m) = CSI ^ (Int.toString n) ^ ";" ^ (Int.toString m) ^ s

  (* Send a "Select Graphic Rendition" command *)
  val SGR = cmd1 "m"

  type color = int

  val (black, red, green, yellow, blue, magenta, cyan, white) = (0, 1, 2, 3, 4, 5, 6, 7)

  val reset = SGR 0
  fun underline true = SGR 4
    | underline false = SGR 24
  fun bold true = SGR 1
    | bold false = SGR 22
  fun blink true = SGR 5
    | blink false = SGR 25
  fun inverse true = SGR 7
    | inverse false = SGR 27

  fun fgColor color = SGR (30 + color)
  fun bgColor color = SGR (40 + color)

  val defaultFgColor = SGR 39
  val defaultBgColor = SGR 40

  fun color cl =
    fgColor cl

  type clear = int
  val (Forward, Backward, Full) = (0, 1, 2)
  val cursorUp = cmd1 "A"
  val cursorDown = cmd1 "B"
  val cursorForward = cmd1 "C"
  val cursorBack = cmd1 "D"
  val cursorNextLine = cmd1 "E"
  val cursorPrevLine = cmd1 "F"
  val cursorSetCol = cmd1 "G"
  val cursorPosition = cmd2 "H"
  val clearScreen = cmd1 "J"
  val clearLine = cmd1 "K"
  val scrollUp = cmd1 "S"
  val scrollDown = cmd1 "T"

  fun cursorVisible true = CSI ^ "?25l"
    | cursorVisible false = CSI ^ "?25h"
end

structure DisableAnsiColors :> ANSI_COLORS =
struct
  type color = unit
  val (black, red, green, yellow, blue, magenta, cyan, white) = ((), (), (), (), (), (), (), ())
  val reset = ""
  fun underline _ = ""
  fun blink _ = ""
  fun bold _ = ""
  fun color _ = ""
end

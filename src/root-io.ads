with Ada.Text_IO; use Ada.Text_IO;

package Root.IO is 

  WIDTH : constant Integer := 40;

  type Color is (Black, Red, Green, Yellow, 
                 Blue, Magenta, Cyan, White, Default,
                 B_Black, B_Red, B_Green, B_Yellow,
                 B_Blue, B_Magenta, B_Cyan, B_White);

  for Color use (
    Black     => 30,
    Red       => 31,
    Green     => 32,
    Yellow    => 33,
    Blue      => 34,
    Magenta   => 35,
    Cyan      => 36,
    White     => 37,
    Default   => 39,
    B_Black   => 90,
    B_Red     => 91,
    B_Green   => 92,
    B_Yellow  => 93,
    B_Blue    => 94,
    B_Magenta => 95,
    B_Cyan    => 96,
    B_White   => 97
    );

  type Style is (None, Dim, Italic, Underline, Strikethrough,
                 Not_Dim, Not_Italic, Not_Underline, Not_Strikethrough);

  for Style use (
    None              => 0,
    Dim               => 2,
    Italic            => 3,
    Underline         => 4,
    Strikethrough     => 9,
    Not_Dim           => 22,
    Not_Italic        => 23,
    Not_Underline     => 24,
    Not_Strikethrough => 29
    );

  ----------------------------
  -- Get Checked User Input 
  --
  -- General Format:
  --  $ Input: <user input>
  ----------------------------

  --function Get_Option  (Num_Opts  : in Integer;
  --                      Options   : in array of String) return Character;
  --function Get_Integer (Low, High : in Integer)         return Integer;
  --function Get_Yes_No  return Boolean;


  ----------------
  -- Formatting --
  ----------------

  -- Centered around WIDTH --
  procedure Put_Line_Centered (S : String);

  procedure Set_Style (FG :    Color; 
                       S  : Style := None);
  procedure Reset_Style;

  -------------------
  -- Common Prints --
  -------------------
  procedure Continue;
  procedure Separator;

  procedure Put_Birdsong;
  procedure Put_Daylight;
  procedure Put_Evening;

  procedure Put_Suit_Opts;

end Root.IO;

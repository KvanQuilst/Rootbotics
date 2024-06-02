-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                             ROOT . IO (Spec)                              --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file specifies the common terminal IO interactions for The           --
-- Rootbotics Assistant.                                                     --
--                                                                           --
-- The Rootbotics Assistant is free software: you can redistribute it and/or --
-- modify it under the terms of the GNU General Public License as published  --
-- by the Free Software Foundation, either version 3 of the License, or (at  --
-- your option) any later version.                                           --
--                                                                           --
-- The Rootbotics Assistant is distributed in the hope that it will be       --
-- useful, but WITHOUT ANY WARRANTY; wihtout even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General  --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Assistant. If not, see                                --
-- <https://www.gnu.org/licenses/>.                                          --
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Root.Faction; use Root.Faction;

package Root.IO is
   package Integer_Set is
      new Ada.Containers.Ordered_Sets (Element_Type => Integer);
   package Character_Set is
      new Ada.Containers.Ordered_Sets (Element_Type => Character);

   WIDTH : constant Integer := 40;

   type Int_Arr     is array (Positive range <>) of Integer;
   type Char_Arr    is array (Positive range <>) of Character;
   type String_Arr  is array (Positive range <>) of Unbounded_String;
   type Boolean_Arr is array (Positive range <>) of Boolean;

   subtype Int_Set  is Integer_Set.Set;
   subtype Char_Set is Character_Set.Set;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   ----------------
   -- Formatting --
   ----------------

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

   type Color_Arr  is array (Positive range <>) of Color;

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

   -- Centered around WIDTH --
   procedure Put_Line_Centered (S : String);

   procedure Set_Style   (FG : Color;
                          S  : Style := None);
   function String_Style (Str : String;
                          FG  : Color;
                          S   : Style := None) return String
      with Inline;
   procedure Reset_Style;

   --------------------------
   -- Common Color Strings --
   --------------------------
   Suit_Str : constant array (Suit'Range) of Unbounded_String :=
      (Fox    => Unbounded (ESC & "[31mFox" & ESC & "[0m"),
       Mouse  => Unbounded (ESC & "[33mMouse" & ESC & "[0m"),
       Rabbit => Unbounded (ESC & "[93mRabbit" & ESC & "[0m"),
       Bird   => Unbounded (ESC & "[94mBird" & ESC & "[0m"));

   Suit_Str_Plain : constant array (Suit'Range) of Unbounded_String :=
      (Fox    => Unbounded ("Fox"),
       Mouse  => Unbounded ("Mouse"),
       Rabbit => Unbounded ("Rabbit"),
       Bird   => Unbounded ("Bird"));

   Suit_Color : constant array (Suit'Range) of Color :=
      (Fox    => Red,
       Mouse  => Yellow,
       Rabbit => B_Yellow,
       Bird   => Blue);

   Phase_Color : constant array (Phase'Range) of Color :=
      (Birdsong => Yellow,
       Daylight => B_Cyan,
       Evening  => B_Black,
       None     => White);

   ----------------------------
   -- Get Checked User Input --
   --                        --
   -- General Format:        --
   --  $ Input: <user input> --
   ----------------------------
   function Get_Option     (Options   : String_Arr) return Character;
   function Get_Options    (Options   : String_Arr) return Char_Set;
   -- Assumes Options are uncolored --
   function Get_Option_HL  (Options    : String_Arr;
                            Opt_Colors : Color_Arr) return Character;
   function Get_Options_HL (Options    : String_Arr;
                            Opt_Colors : Color_Arr) return Char_Set;
   function Get_Integer    (Low, High : Integer)    return Integer;
   function Get_Integers   (Low, High : Integer)    return Int_Set;

   function Get_Yes_No return Boolean;
   function Get_Secret return Character;

   -----------------
   -- Common Gets --
   -----------------
   function Get_Suit_Opt return Suit;
   function Get_Clearing_Suit_Opt return Clearing_Suit;
   function Get_Rule (Name : String; Clear : Priority) return Boolean;

   ---------------------
   -- Cursor Controls --
   ---------------------
   procedure Cursor_Home
      with Inline;
   procedure Cursor_Set (Line : Positive; Column : Natural)
      with Inline;
   procedure Cursor_Line_Move   (Num_Lines : Integer);
   procedure Cursor_Column_Move (Num_Columns : Integer);
   procedure Cursor_Column_Set  (Column : Natural)
      with Inline;

   ----------------------------
   -- Erase Functions --
   ----------------------------
   procedure Erase_Screen
      with Inline;

   -------------------
   -- Common Prints --
   -------------------
   procedure Continue;
   procedure Separator;

   procedure Put_Phase (Time : Phase; Action : String := "");
   procedure Put_Prompt (Put_Logo      : access procedure;
                         Put_State     : access procedure;
                         Units         : Warrior_Arr;
                         Buildings     : Building_Arr;
                         Rule          : Rule_Arr;
                         Current_Order : Suit;
                         Phase         : access procedure := null;
                         Tokens        : Token_Arr := (others => False));

   -------------------
   -- Rootbotics IO --
   -------------------
   procedure Put_Title;
   procedure Put_Title_Prompt;

   Title : constant array (Integer range 1 .. 9) of String (1 .. 40) :=
      ("  ____________________________________  ",
       "//                                    \\",
       "| \=====\\     ---       ---  \======/ |",
       "|  ||    ||  //   \\   //   \\   ||    |",
       "|  || /==<  ||     || ||     ||  ||    |",
       "|  ||    ||  \\   //   \\   //   ||    |",
       "|  |\_    \\   ---       ---     /\_/  |",
       "|  A Game of Woodland Might and Right  |",
       "\\____________________________________//");

end Root.IO;

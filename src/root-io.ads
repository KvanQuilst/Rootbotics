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
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with IO_Utils.Ansi; use IO_Utils.Ansi;
with IO_Utils.Strings; use IO_Utils.Strings;

with Root.Color; use Root.Color;
with Root.Faction; use Root.Faction;

package Root.IO is
   package Integer_Set is
      new Ada.Containers.Ordered_Sets (Element_Type => Integer);

   WIDTH : constant Integer := 40;

   subtype Int_Set  is Integer_Set.Set;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   -- Centered around WIDTH --
   procedure Put_Line_Centered (S : String);

   --------------------------
   -- Common Color Strings --
   --------------------------
   Suit_Str_Plain : constant array (Suit'Range) of Unbounded_String :=
      (Fox    => Unbounded ("Fox"),
       Mouse  => Unbounded ("Mouse"),
       Rabbit => Unbounded ("Rabbit"),
       Bird   => Unbounded ("Bird"));

   -- This type shouldn't need to be referenced... --
   type Suit_Color_Arr is array (Suit'Range) of Color_Elem;
   function Suit_Color return Suit_Color_Arr is
      (Fox    => Fox_Color,
       Mouse  => Mouse_Color,
       Rabbit => Rabbit_Color,
       Bird   => Bird_Color);

   -- This type shouldn't need to be referenced... --
   type Suit_Str_Arr is array (Suit'Range) of Unbounded_String;
   function Suit_Str return Suit_Str_Arr is
      (Fox    => Unbounded (Set_Fg (To_String (Suit_Str_Plain (Fox)),
                                    Suit_Color (Fox))),
       Mouse  => Unbounded (Set_Fg (To_String (Suit_Str_Plain (Mouse)),
                                    Suit_Color (Mouse))),
       Rabbit => Unbounded (Set_Fg (To_String (Suit_Str_Plain (Rabbit)),
                                    Suit_Color (Rabbit))),
       Bird   => Unbounded (Set_Fg (To_String (Suit_Str_Plain (Bird)),
                                    Suit_Color (Bird))));

   -- This type shouldn't need to be referenced... --
   type Phase_Color_Arr is array (Phase'Range) of Color_Elem;
   function Phase_Color return Phase_Color_Arr is
      (Birdsong => Birdsong_Color,
       Daylight => Daylight_Color,
       Evening  => Evening_Color,
       None     => Default_Color);

   ----------------------------
   -- Get Checked User Input --
   --                        --
   -- General Format:        --
   --  $ Input: <user input> --
   ----------------------------
   function Get_Integers   (Low, High : Integer)    return Int_Set;

   function Get_Yes_No return Boolean;
   function Get_Secret return Character;

   -----------------
   -- Common Gets --
   -----------------
   function Get_Suit_Opt return Suit;
   function Get_Turn_Order return Suit;
   function Get_Clearing_Suit_Opt return Clearing_Suit;
   function Get_Rule (Name  : String;
                      Clear : Priority;
                      Rule  : Rule_Arr) return Boolean;

   -------------------
   -- Common Prints --
   -------------------
   procedure Put_Phase (Time : Phase; Action : String := "");
   procedure Put_Prompt (Put_Logo      : access procedure;
                         Put_State     : access procedure;
                         Units         : Warrior_Arr;
                         Buildings     : Building_Arr;
                         Rule          : Rule_Arr;
                         Current_Order : Suit;
                         Phase         : access procedure := null;
                         Tokens        : Token_Arr := (others => False));
   procedure Put_Score (Score : Integer;
                        Name  : String);

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

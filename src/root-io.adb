-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                             ROOT . IO (Body)                              --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the terminal IO interactions for --
-- The Rootbotics Assistant.                                                 --
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
with Ada.Text_IO; use Ada.Text_IO;

with Root.Maps; use Root.Maps;

package body Root.IO is
   package Int_IO is
      new Integer_IO (Integer); use Int_IO;

  ----------------------------
  -- Get Checked User Input --
  ----------------------------

   procedure Put_Options (Options : String_Arr) is
   begin
      pragma Assert (Options'Length /= 0);

      -- Print options a - (a + Num_Opts) --
      Separator;
      for I in Options'Range loop
         Put_Line (" " & Character'Val (96 + I) & ". " &
           To_String (Options (I)));
      end loop;
      Separator;
   end Put_Options;

   function Get_Option (Options  : String_Arr) return Character is
      C    : Character;

      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      Put_Options (Options);

      -- Check character is between 'a' and '(a + Num_Opts)' --
      loop
         Put ("Option: ");
         Get (C);
         Line := To_Unbounded_String (Get_Line);
         exit when Character'Pos (C) - 96 > 0 and then
                   Character'Pos (C) - 96 <= Options'Length;
         Put_Line ("Invalid option!");
      end loop;

      return C;
   end Get_Option;

   function Get_Options (Options  : String_Arr) return Char_Set is
      Opts    : Char_Set;
      Line    : Unbounded_String;
      Invalid : Boolean;
   begin
      Put_Options (Options);

      Put_Line ("Enter the options applicable. Press enter for 'none'...");

      loop
         Invalid := False;

         Put ("Options: ");

         Line := To_Unbounded_String (Get_Line);

         -- Get number of options / determine invalid --
         for C of To_String (Line) loop
            if C >= 'a' and then C <= Character'Val (96 + Options'Length)
            then
               Opts.Include (C);
            elsif C /= ' ' then
               Invalid := True;
            end if;
         end loop;

         exit when Invalid = False;
         Put_Line ("Invalid response!");
      end loop;

      return Opts;
   end Get_Options;

   function Get_Integer (Low, High : Integer) return Integer is
      C    : Character;
      EOL  : Boolean;
      Val  : Integer := -1;

      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      loop
         Put ("Response: ");
         Look_Ahead (C, EOL);
         if C >= '0' and then C <= '9' then
            Get (Val);
         end if;

         exit when Val >= Low and then Val <= High;

         Put_Line ("Invalid input!");
         Line := To_Unbounded_String (Get_Line);
      end loop;
      Line := To_Unbounded_String (Get_Line);
      return Val;
   end Get_Integer;

   function Get_Integers (Low, High : Integer) return Int_Set is
      Ints  : Int_Set;
      Line  : Unbounded_String;
      Val   : Integer;
      Last  : Integer := 1;
      Invalid : Boolean;
   begin
      Put_Line ("Enter values. Press enter for 'none'");

      loop
         Invalid := False;
         Last := 1;

         Put ("Values: ");
         Line := To_Unbounded_String (Get_Line);

         -- Check if space separated numbers --
         for C of To_String (Line) loop
            Invalid := Invalid or else (C /= ' ' and then C not in '0' .. '9');
         end loop;

         -- Add values to Ints --
         while Last <= Length (Line) loop
            if Element (Line, Last) in '0' .. '9' then
               Get (Slice (Line, Last, Length (Line)), Val, Last);
               Invalid := Invalid or else Val not in Low .. High;

               exit when Invalid;

               Ints.Include (Val);
            end if;
            Last := Last + 1;
         end loop;

         exit when Invalid = False;
         Put_Line ("Invalid response!");
      end loop;

      return Ints;
   end Get_Integers;

   function Get_Yes_No return Boolean is
      C    : Character;

      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      loop
         Put ("Response (y/n): ");
         Get (C);
         Line := To_Unbounded_String (Get_Line);

         exit when C = 'y' or else C = 'Y' or else
                   C = 'n' or else C = 'N';

         Put_Line ("Invalid input!");
      end loop;

      return C = 'y' or else C = 'Y';
   end Get_Yes_No;

   -----------------
   -- Common Gets --
   -----------------

   function Get_Suit_Opt return Suit is
      S : constant String_Arr := (
         To_Unbounded_String (Fox),
         To_Unbounded_String (Mouse),
         To_Unbounded_String (Rabbit),
         To_Unbounded_String (Bird)
         );
   begin
      return Suit'Val (Character'Pos (Get_Option (S)) - 97);
   end Get_Suit_Opt;

   function Get_Clearing_Suit_Opt return Clearing_Suit is
      S : constant String_Arr := (
         To_Unbounded_String (Fox),
         To_Unbounded_String (Mouse),
         To_Unbounded_String (Rabbit)
         );
   begin
      return Suit'Val (Character'Pos (Get_Option (S)) - 97);
   end Get_Clearing_Suit_Opt;

   function Get_Rule (Name : String; Clear : Priority) return Boolean is
   begin
      Put_Line ("Does the " & Name & " rule clearing" & Clear'Image & "?");
      return Get_Yes_No;
   end Get_Rule;

   ----------------
   -- Formatting --
   ----------------

   procedure Put_Line_Centered (S : String) is
      Start, Length : Integer;
      Escape : Boolean := False;
   begin
      -- Account for terminal escape codes --
      Length := S'Length;
      for I in S'Range loop
         if Escape then
            Length := Length - 1;
            Escape := S (I) /= 'm';
         else
            Escape := S (I) = ESC;
         end if;
      end loop;

      Start := (WIDTH / 2) - (Length / 2);
      for I in 1 .. Start loop
         Put (" ");
      end loop;
      Put_Line (S);
   end Put_Line_Centered;

   procedure Set_Style (FG : Color;
                        S  : Style := None) is
   begin
      Put (ESC & "[");

      -- Style --
      if S /= None then
         Put (Style'Enum_Rep (S), 0);
         Put (";");
      end if;

      -- FG --
      Put (Color'Enum_Rep (FG), 0);
      Put ("m");
   end Set_Style;

   function String_Style (Str : String;
                          FG  : Color;
                          S   : Style := None) return String is
      Out_Str : Unbounded_String;
      Val     : String (1 .. 2);
   begin
      Out_Str := To_Unbounded_String (ESC & "[");

      -- Style --
      if S /= None then
         if Style'Enum_Rep (S) < 10 then
            declare
               Val : String (1 .. 1);
            begin
               Put (Val, Style'Enum_Rep (S));
               Out_Str := Out_Str & Val & ";";
            end;
         else
            Put (Val, Style'Enum_Rep (S));
            Out_Str := Out_Str & Val & ";";
         end if;
      end if;

      -- FG --
      Put (Val, Color'Enum_Rep (FG));
      Out_Str := Out_Str & Val & "m" & Str & ESC & "[0m";

      return To_String (Out_Str);
   end String_Style;

   procedure Reset_Style is
   begin
      Put (ESC & "[0m");
   end Reset_Style;

   function Suit_Color (S : Suit) return Color is
      (case S is
         when Root.Fox => Fox_Color,
         when Root.Mouse => Mouse_Color,
         when Root.Rabbit => Rabbit_Color,
         when Root.Bird => Bird_Color);

   ---------------------
   -- Cursor Controls --
   ---------------------
   procedure Cursor_Home is
   begin
      Put (ESC & "[H");
   end Cursor_Home;

   procedure Cursor_Set (Line : Positive; Column : Natural) is
   begin
      Put (ESC & "[");
      Put (Line, Width => 0);
      Put (";");
      Put (Column, Width => 0);
      Put ("H");
   end Cursor_Set;

   procedure Cursor_Line_Move (Num_Lines : Integer) is
      Val : constant Natural := (if Num_Lines < 0
                                 then Natural (0 - Num_Lines)
                                 else Natural (Num_Lines));
   begin
      Put (ESC & "[");
      Put (Val, Width => 0);
      if Num_Lines < 0 then
         Put ("A");
      elsif Num_Lines > 0 then
         Put ("B");
      end if;
   end Cursor_Line_Move;

   procedure Cursor_Column_Move (Num_Columns : Integer) is
      Val : constant Natural := (if Num_Columns < 0
                                 then Natural (0 - Num_Columns)
                                 else Natural (Num_Columns));
   begin
      Put (ESC & "[");
      Put (Val, Width => 0);
      if Num_Columns < 0 then
         Put ("D");
      elsif Num_Columns > 0 then
         Put ("C");
      end if;
   end Cursor_Column_Move;

   procedure Cursor_Column_Set (Column : Natural) is
   begin
      Put (ESC & "[");
      Put (Column, Width => 0);
      Put ("G");
   end Cursor_Column_Set;

   ---------------------
   -- Erase Functions --
   ---------------------

   procedure Erase_Screen is
   begin
      Put (ESC & "[2J");
   end Erase_Screen;

   -------------------
   -- Common Prints --
   -------------------

   procedure Continue is
      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      New_Line;
      Put ("Press enter to continue...");
      Line := To_Unbounded_String (Get_Line);
      New_Line;
   end Continue;

   procedure Separator is
   begin
      Put_Line (To_String (WIDTH * "-"));
   end Separator;

   procedure Put_Birdsong is
   begin
      New_Line;
      Put_Line_Centered (To_String (16 * "-"));
      Set_Style (FG => Yellow);
      Put_Line_Centered ("Birdsong");
      Reset_Style;
      Put_Line_Centered (To_String (16 * "-"));
      New_Line;
   end Put_Birdsong;

   procedure Put_Daylight is
   begin
      New_Line;
      Put_Line_Centered (To_String (16 * "-"));
      Set_Style (FG => B_Cyan);
      Put_Line_Centered ("Daylight");
      Reset_Style;
      Put_Line_Centered (To_String (16 * "-"));
      New_Line;
   end Put_Daylight;

   procedure Put_Evening is
   begin
      New_Line;
      Put_Line_Centered (To_String (15 * "-"));
      Set_Style (FG => B_Black);
      Put_Line_Centered ("Evening");
      Reset_Style;
      Put_Line_Centered (To_String (15 * "-"));
      New_Line;
   end Put_Evening;

   procedure Put_Prompt (Put_Logo      : access procedure;
                         Put_State     : access procedure;
                         Units         : Warrior_Arr;
                         Buildings     : Building_Arr;
                         Rule          : Rule_Arr;
                         Current_Order : Suit;
                         Time          : Phase := None;
                         Tokens        : Token_Arr := (others => False)) is
   begin
      -- Logo --
      Erase_Screen;
      Cursor_Home;
      Put_Logo.all;

      -- Map --
      Put_Map (Units, Buildings, Rule, Tokens);
      New_Line;
      Separator;

      -- State --
      Put_State.all;
      New_Line;
      Put ("    Current Order: ");
      Put_Line ((case Current_Order is
                  when Root.Fox    => Root.IO.Fox,
                  when Root.Rabbit => Root.IO.Rabbit,
                  when Root.Mouse  => Root.IO.Mouse,
                  when Root.Bird   => Root.IO.Bird));
      Separator;
      case Time is
         when Birdsong => Put_Birdsong;
         when Daylight => Put_Daylight;
         when Evening  => Put_Evening;
         when None     => null;
      end case;
   end Put_Prompt;

end Root.IO;

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
with IO_Utils.User_IO; use IO_Utils.User_IO;

with Root.Maps; use Root.Maps;

package body Root.IO is
   package Int_IO is
      new Integer_IO (Integer); use Int_IO;

  ----------------------------
  -- Get Checked User Input --
  ----------------------------

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

   function Get_Secret return Character is
      C : Character;
   begin
      Put (ESC & "[8m");
      Get_Immediate (C);
      Put (ESC & "[28m");
      return C;
   end Get_Secret;

   -----------------
   -- Common Gets --
   -----------------

   function Get_Suit_Opt return Suit is
      C : constant Character := Get_Option ((Suit_Str_Plain (Fox),
                                             Suit_Str_Plain (Rabbit),
                                             Suit_Str_Plain (Mouse),
                                             Suit_Str_Plain (Bird)),
                                            (Suit_Color (Fox),
                                             Suit_Color (Rabbit),
                                             Suit_Color (Mouse),
                                             Suit_Color (Bird)));
   begin
      return Suit'Val (Character'Pos (C) - 97);
   end Get_Suit_Opt;

   function Get_Turn_Order return Suit is
      S : Suit;
   begin
      Put_Line ("What is the order of this turn?");
      S := Get_Suit_Opt;
      Continue;
      return S;
   end Get_Turn_Order;

   function Get_Clearing_Suit_Opt return Clearing_Suit is
      C : constant Character := Get_Option ((Suit_Str_Plain (Fox),
                                             Suit_Str_Plain (Rabbit),
                                             Suit_Str_Plain (Mouse)),
                                            (Suit_Color (Fox),
                                             Suit_Color (Rabbit),
                                             Suit_Color (Mouse),
                                             Suit_Color (Bird)));
   begin
      return Suit'Val (Character'Pos (C) - 97);
   end Get_Clearing_Suit_Opt;

   function Get_Rule (Name  : String;
                      Clear : Priority;
                      Rule  : Rule_Arr) return Boolean is
   begin
      if Rule (Clear) then
         return True;
      end if;
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

   -------------------
   -- Common Prints --
   -------------------

   procedure Put_Phase (Time : Phase; Action : String := "") is
      Width : constant Positive := Positive'Max (Action'Length, 12) + 4;
   begin
      if Time = None then
         return;
      end if;

      Set_Fg (Phase_Color (Time));
      Put_Line_Centered (To_String (Width * "-"));
      case Time is
         when Birdsong => Put_Line_Centered ("Birdsong");
         when Daylight => Put_Line_Centered ("Daylight");
         when Evening  => Put_Line_Centered ("Evening");
         when None     => null;
      end case;
      if Action'Length > 0 then
         Reset_All;
         Put_Line_Centered (Action);
         Set_Fg (Phase_Color (Time));
      end if;
      Put_Line_Centered (To_String (Width * "-"));
      Reset_All;
      New_Line;
   end Put_Phase;

   procedure Put_Prompt (Put_Logo      : access procedure;
                         Put_State     : access procedure;
                         Units         : Warrior_Arr;
                         Buildings     : Building_Arr;
                         Rule          : Rule_Arr;
                         Current_Order : Suit;
                         Phase         : access procedure := null;
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
      Put_Line (To_String (Suit_Str (Current_Order)));
      Separator;
      New_Line;
      if Phase /= null then
         Phase.all;
      end if;
   end Put_Prompt;

   procedure Put_Score (Score : Integer;
                        Name  : String) is
   begin
      Put ("Score +");
      Put (Score);
      if Score = 1 then
         Put_Line (" point for the " & Name & ".");
      else
         Put_Line (" points for the " & Name & ".");
      end if;
   end Put_Score;

   -------------------
   -- Rootbotics IO --
   -------------------
   procedure Put_Title is
      First : Boolean := True;
   begin
      Set_Fg (Yellow);
      Put_Line (Title (1));
      Put_Line (Title (2));
      for L of Title (3 .. Title'Last - 2) loop
         Put (L (1 .. 2));
         Set_Fg (Suit_Color (Fox));
         Put (L (3 .. 12));
         Set_Fg (Suit_Color (Rabbit));
         Put (L (13 .. 22));
         Reset_All;
         Set_Fg (Suit_Color (Mouse));
         if First then
            Put (L (23 .. 30));
            Set_Fg (Suit_Color (Bird));
            Put (L (31 .. 38));
            First := False;
         else
            Put (L (23 .. 31));
            Set_Fg (Suit_Color (Bird));
            Put (L (32 .. 38));
         end if;
         Set_Fg (Yellow);
         Put (L (39 .. 40));
         New_Line;
      end loop;
      Put (Title (Title'Last - 1) (1 .. 2));
      Reset_All;
      Put (Title (Title'Last - 1) (3 .. 38));
      Set_Fg (Yellow);
      Put_Line (Title (Title'Last - 1) (39 .. 40));
      Put_Line (Title (Title'Last));
      Reset_All;
   end Put_Title;

   procedure Put_Title_Prompt is
   begin
      -- Title --
      Erase_Screen;
      Cursor_Home;
      Put_Title;
      New_Line;
   end Put_Title_Prompt;
end Root.IO;

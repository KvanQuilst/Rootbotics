-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                            ROOT . MAPS (Body)                             --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation for the map related components of   --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with IO_Utils.Ansi; use IO_Utils.Ansi;

with Root.Color;
with Root.IO; use Root.IO;

package body Root.Maps is
   package Int_IO is new Integer_IO (Integer);

   function Clearings return Clearing_Arr is (Map);

   function Filter_Clearings (S : Suit) return Int_Arr is
      F_Clears : Int_Arr (1 .. 4);
      Index    : Positive := 1;
   begin
      if S = Bird then
         return (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
      end if;

      for I in Priority'Range loop
         if Map (I).C_Suit = S then
            F_Clears (Index) := I;
            Index := Index + 1;
         end if;
      end loop;
      return F_Clears;
   end Filter_Clearings;

   function Coords return Coordinates is
      (case Map_In_Play is
         when Fall     => Fall_Clearing_Coords,
         when Winter   => Winter_Clearing_Coords,
         when Lake     => Lake_Clearing_Coords,
         when Mountain => Mountain_Clearing_Coords);

   function Text_Map return Map_Text is
      (case Map_In_Play is
         when Fall     => Fall_Map_Base,
         when Winter   => Winter_Map_Base,
         when Lake     => Lake_Map_Base,
         when Mountain => Mountain_Map_Base);

   function Query_Suit (Prio : Priority) return Clearing_Suit is
      procedure Put_Clearing (Line, Col : Natural;
                              S : Clearing_Suit; Prio : Priority) is
         C      : constant Color_Elem := Suit_Color (S);
         S_Char : constant Character  := (case S is
                                             when Fox    => 'F',
                                             when Mouse  => 'M',
                                             when Rabbit => 'R');
      begin
         Cursor_Line_Move (Line);
         Cursor_Col_Set (Col);
         Set_Fg (C);
         Put ("@---@");
         Cursor_Line_Move (1);
         Cursor_Col_Set (Col);

         Put ("|");
         Reset_All;
         Put (" _ ");
         Set_Fg (C);
         Put ("|");
         Cursor_Line_Move (1);
         Cursor_Col_Set (Col);

         Put (S_Char & "--");
         if Prio < 10 then
            Put ("-");
         end if;
         Set_Fg (Root.Color.Light_Grey);
         Int_IO.Put (Prio, Width => 0);
         Reset_All;
         Cursor_Line_Move (0 - Line - 2);
         Cursor_Col_Set (Col);
      end Put_Clearing;

      B_Col : constant := (Root.IO.WIDTH - Map_Width) / 2 + 2;
   begin
      Erase_Screen;
      Cursor_Home;
      Put_Line_Centered (Map_In_Play'Image);
      for L of Text_Map loop
         Put_Line (To_String ((B_Col - 1) * ' ') & L);
      end loop;
      Cursor_Line_Move (0 - Text_Map'Length);
      Cursor_Col_Move (B_Col);

      for I in Priority'Range loop
         if Set_Clearings (I) /= Bird then
            Put_Clearing (Coords (I).x, B_Col + Coords (I).y,
                          Set_Clearings (I), I);
         end if;
      end loop;
      Cursor_Line_Move (Text_Map'Length);
      New_Line;
      Separator;
      New_Line;

      Put_Line ("What is the suit of clearing" & Prio'Image & "?");
      Set_Clearings (Prio) := Get_Clearing_Suit_Opt;
      return Set_Clearings (Prio);
   end Query_Suit;

   procedure Init_Map (Name : Map_Name) is
      Counts : array (Clearing_Suit'Range) of Natural := (others => 0);
   begin
      Map_In_Play := Name;

      case Name is
         when Fall => Map := Fall_Map_Clean;
         when Winter => Map := Winter_Map_Clean;
         when Lake => Map := Lake_Map_Clean;
         when Mountain => Map := Mountain_Map_Clean;
      end case;

      if Name /= Fall then
         for C of Set_Clearings loop
            C := Bird;
         end loop;

         loop
            for C in Priority'Range loop
               Map (C).C_Suit := Query_Suit (C);
               Counts (Map (C).C_Suit) := Counts (Map (C).C_Suit) + 1;
            end loop;

            exit when (for all S of Counts => S = 4);

            Put_Line ("Each suit should have 4 associated clearings...");
            for S of Counts loop
               S := 0;
            end loop;
            Continue;
         end loop;
      end if;
   end Init_Map;

   ------------------
   -- Map Printing --
   ------------------

   -- Line, Col : Relative Positioning --
   procedure Clearing_Box (Line : Natural; Col, Units, Buildings : Natural;
                           Rule : Boolean; Tok : Boolean;
                           Clear : Clearing; Pri : Priority) is
      C : constant Color_Elem := Suit_Color (Clear.C_Suit);
      S : constant Character  := (case Clear.C_Suit is
                                    when Fox    => 'F',
                                    when Mouse  => 'M',
                                    when Rabbit => 'R');
   begin
      Cursor_Line_Move (Line);
      Cursor_Col_Set (Col);
      if Buildings > 0 then
         Int_IO.Put (Buildings, Width => 0);
      else
         Put ("@");
      end if;
      Set_Fg (C);
      Put ("---");
      if Rule then
         Reset_All;
         Cursor_Col_Move (-2);
         Put ("^");
         Set_Fg (C);
         Cursor_Col_Move (1);
      end if;

      if Tok then
         Reset_All;
         Put ("T");
         Set_Fg (C);
         Cursor_Col_Move (-5);
      else
         Cursor_Col_Move (-4);
      end if;

      Cursor_Line_Move (1);

      Put ("| ");
      Reset_All;
      if Units >= 10 then
         Int_IO.Put (Units, Width => 0);
      else
         Int_IO.Put (Units, Width => 0);
         Put (" ");
      end if;
      Set_Fg (C);
      Put ("|");
      Cursor_Line_Move (1);
      Cursor_Col_Move (-5);

      Put (S & "--");
      if Pri < 10 then
         Put ("-");
      end if;
      Set_Fg (Root.Color.Light_Grey);
      Int_IO.Put (Pri, Width => 0);
      Reset_All;
      Cursor_Line_Move (0 - Line - 2);
      Cursor_Col_Set (Col);
   end Clearing_Box;

   procedure Put_Map (Units     : Warrior_Arr;
                      Buildings : Building_Arr;
                      Rule      : Rule_Arr;
                      Tokens    : Token_Arr := (others => False)) is
      B_Col    : constant := (Root.IO.WIDTH - Map_Width) / 2 + 2;
   begin
      Put_Line_Centered (Map_In_Play'Image);

      -- Print base map and return to TR corner --
      for L of Text_Map loop
         Put_Line (To_String ((B_Col - 1) * ' ') & L);
      end loop;
      Cursor_Line_Move (0 - Text_Map'Length);
      Cursor_Col_Move (B_Col);

      -- Print map details and numbers --
      for I in Priority'Range loop
         Clearing_Box (Coords (I).x, B_Col + Coords (I).y,
                       Units (I), Buildings (I), Rule (I),
                       Tokens (I), Map (I), I);
      end loop;
      Cursor_Line_Move (Text_Map'Length);
   end Put_Map;

end Root.Maps;

-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Body)                            --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the common faction-related       --
-- subroutines used throughout The Rootbotics Assistnat.                     --
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
with IO_Utils.Ansi; use IO_Utils.Ansi;
with IO_Utils.User_IO; use IO_Utils.User_IO;

with Root.Color;
with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package body Root.Faction is

   ----------------
   -- Faction IO --
   ----------------
   procedure Put_Logo (Name       : String;
                       Logo       : Logo_Arr;
                       Logo_Width : Positive) is
      B_Col : constant Integer := (WIDTH - Logo_Width) / 2 + 2;
   begin
      Put_Line_Centered (Name);
      Cursor_Line_Move (9);
      Put_Line (To_String (WIDTH * '-'));
      Cursor_Line_Move (-9);
      for L of Logo loop
         Cursor_Col_Set (B_Col);
         Put (To_String (L));
         Cursor_Line_Move (1);
      end loop;
      New_Line;
   end Put_Logo;

   ---------------------------------
   -- Faction Resrouce Management --
   ---------------------------------
   function Check_Difficulty (Name : String) return Difficulty is
      Diff : Character range 'a' .. 'd';
   begin
      Put_Line ("What difficulty will the " & Name & " play at?");
      Diff := Get_Option ((To_Unbounded_String ("Easy"),
                           To_Unbounded_String ("Normal (no change)"),
                           To_Unbounded_String ("Challenging"),
                           To_Unbounded_String ("Nightmare")),
                          (Root.Color.Green,
                           Root.Color.Default,
                           Root.Color.Blue,
                           Root.Color.Red));
      return (case Diff is
               when 'a' => Easy,
               when 'b' => Normal,
               when 'c' => Challenging,
               when 'd' => Nightmare);
   end Check_Difficulty;

   function Check_Warriors (Prompt       : access procedure;
                            Supply       : in out   Natural;
                            Map_Warriors : in out   Warrior_Arr;
                            Max_Warriors :          Integer) return Natural is
      Lost : Natural := 0;
   begin
      Prompt.all;
      Put_Line ("Does the number of warriors match for each clearing?");
      if not Get_Yes_No then
         Prompt.all;
         Put_Line ("Which clearings are incorrect?");
         declare
            Clearings : constant Int_Set := Get_Integers (1, 12);
            Warriors  : Integer;
            S         : Integer := Supply;
         begin
            loop
               Lost := 0;
               for C of Clearings loop
                  Prompt.all;
                  Put_Line ("What is the number of warriors in clearing" &
                             C'Image & "?");
                  Warriors := Get_Integer (0, Max_Warriors);
                  if Warriors < Map_Warriors (C) then
                     Lost := Lost + (Map_Warriors (C) - Warriors);
                  end if;
                  S := S + (Map_Warriors (C) - Warriors);
                  Map_Warriors (C) := Warriors;
               end loop;

               exit when S >= 0 and then S <= Max_Warriors;

               Put_Line ("The provided values don't add up, let's try again.");
               Continue;
            end loop;
            Supply := S;
         end;
      end if;
      return Lost;
   end Check_Warriors;

   function Check_Buildings (Prompt : access procedure;
                             Supply : in out Suit_Build_Supply;
                             Builds : in out Building_Arr;
                             Max_Builds : Integer) return Natural is
      Lost : Natural := 0;
   begin
      Prompt.all;
      Put_Line ("Does the number of buildings match for each clearing?");
      if not Get_Yes_No then
         Prompt.all;
         Put_Line ("Which clearings are incorrect?");
         declare
            Clearings : constant Int_Set := Get_Integers (1, 12);
            S         : Suit;
            Buildings : Integer;
            New_Supply : Suit_Build_Supply := (Supply (Fox),
                                               Supply (Mouse),
                                               Supply (Rabbit));
         begin
            loop
               Lost := 0;
               for C of Clearings loop
                  S := Root.Maps.Clearings (C).C_Suit;

                  Prompt.all;
                  Put_Line ("What is the number of buildings in clearing" &
                            C'Image & "?");
                  Buildings :=
                     Get_Integer (0, Root.Maps.Clearings (C).Buildings);
                  if Buildings < Builds (C) then
                     Lost := Lost + (Builds (C) - Buildings);
                  end if;
                  New_Supply (S) := New_Supply (S) + (Builds (C) - Buildings);
                  Builds (C) := Buildings;
               end loop;

               exit when
                  (for all I of New_Supply => I >= 0 and then I <= Max_Builds);

               Put_Line ("The provided values don't add up, let's try again.");
               Continue;
            end loop;
            for S in New_Supply'Range loop
               Supply (S) := New_Supply (S);
            end loop;
         end;
      end if;
      return Lost;
   end Check_Buildings;

   procedure Check_Tokens (Prompt : access procedure;
                           Supply : in out Natural;
                           Tokens : in out Token_Arr) is
   begin
      Prompt.all;
      Put_Line ("Does the number of tokens match for each clearaing?");
      if not Get_Yes_No then
         Prompt.all;
         Put_Line ("Which clearings are incorrect?");
         declare
            Clearings : constant Int_Set := Get_Integers (1, 12);
         begin
            for C of Clearings loop
               Supply := Supply + (if Tokens (C) then 1 else (-1));
               Tokens (C) := not Tokens (C);
            end loop;
         end;
      end if;
   end Check_Tokens;

   procedure Check_Rule (Prompt : access procedure;
                         Rule   : in out Rule_Arr) is
   begin
      Prompt.all;
      Put_Line ("Does the rule match for each clearings?");
      if not Get_Yes_No then
         Prompt.all;
         Put_Line ("Which clearings are incorrect?");
         declare
            Clearings : constant Int_Set := Get_Integers (1, 12);
         begin
            for C of Clearings loop
               Rule (C) := not Rule (C);
            end loop;
         end;
      end if;
   end Check_Rule;

   procedure Move_Warriors (Map_Warriors : in out Warrior_Arr;
                            Rule         : in out Rule_Arr;
                            To, From     :        Priority;
                            Num_Warriors :        Natural;
                            Name         :        String;
                            Prompt       : access procedure) is
   begin
      pragma Assert (Map_Warriors (From) >= Num_Warriors);
      pragma Assert ((for some N of Clearings (From).Neighbors => N = To));

      if Map_Warriors (From) = 0 then
         return;
      end if;

      if not Rule (To) or else
         not Rule (From)
      then
         return;
      end if;

      if Num_Warriors = Map_Warriors (From) then
         Put_Line ("Move all warriors from clearing" & From'Image
                 & " to clearing" & To'Image & ".");
      else
         Put_Line ("Move" & Num_Warriors'Image & " warriors from clearing"
                 & From'Image & " to clearing" & To'Image & ".");
      end if;
      Continue;

      Map_Warriors (To) := Map_Warriors (To) + Num_Warriors;
      Map_Warriors (From) := Map_Warriors (From) - Num_Warriors;
      if not Rule (To) then
         Prompt.all;
         Put_Line ("Do the " & Name & " rule clearing" & To'Image & " now?");
         Rule (To) := Get_Yes_No;
         Continue;
      end if;

      if Map_Warriors (From) /= 0 and then Rule (From)
      then
         Prompt.all;
         Put_Line ("Do the " & Name & " still rule clearing"
                 & From'Image & "?");
         Rule (From) := Get_Yes_No;
         Continue;
      end if;
   end Move_Warriors;

   procedure Deploy_Warriors (Supply       : in out Natural;
                              Map_Warriors : in out Warrior_Arr;
                              Clear        :        Priority;
                              Num_Warriors :        Positive) is
   begin
      if Supply >= Num_Warriors then
         Put_Line ("Place" & Num_Warriors'Image & " warriors in clearing" &
                   Clear'Image & ".");
         Map_Warriors (Clear) := Map_Warriors (Clear) + Num_Warriors;
         Supply := Supply - Num_Warriors;
      elsif Supply > 0 then
         Put_Line ("Place" & Supply'Image & " warriors in clearing" &
                   Clear'Image & ".");
         Map_Warriors (Clear) := Map_Warriors (Clear) + Supply;
         Supply := 0;
      else
         Put_Line ("Unable to place warriors: warrior supply is depleted!");
      end if;
      Continue;
   end Deploy_Warriors;

   function Deploy_Building (Supply     : in out Natural;
                             Map_Builds : in out Building_Arr;
                             Clear      :        Priority;
                             Build_Type :        String) return Boolean is
      Ret : Boolean := True;
   begin
      Put_Line ("Does clearing" & Clear'Image & " have building space?");
      if not Get_Yes_No then
         return False;
      end if;

      New_Line;
      if Supply > 0 and then
         Map_Builds (Clear) < Clearings (Clear).Buildings
      then
         Put_Line ("Place a " & Build_Type & " in clearing" &
                   Clear'Image & ".");
         Map_Builds (Clear) := Map_Builds (Clear) + 1;
         Supply := Supply - 1;
      else
         Put_Line ("Unable to place building: building supply is depleted!");
         Ret := False;
      end if;
      Continue;
      return Ret;
   end Deploy_Building;

end Root.Faction;

-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Body)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
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
         Cursor_Column_Set (B_Col);
         Put (To_String (L));
         Cursor_Line_Move (1);
      end loop;
      New_Line;
   end Put_Logo;

   ---------------------------------
   -- Faction Resrouce Management --
   ---------------------------------
   function Check_Warriors (Prompt : access procedure (Time : Phase := None);
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

   function Check_Buildings (Prompt : access procedure (Time : Phase := None);
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

   procedure Check_Tokens (Prompt : access procedure (Time : Phase := None);
                           Supply : in out Natural;
                           Tokens : in out Token_Arr) is null;

   procedure Check_Rule (Prompt : access procedure (Time : Phase := None);
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
   end Deploy_Warriors;

   procedure Deploy_Building (Supply     : in out Natural;
                              Map_Builds : in out Building_Arr;
                              Clear      :        Priority;
                              Build_Type :        String) is
   begin
      if Supply > 0 and then Map_Builds (Clear) < 3 then
         Put_Line ("Place a " & Build_Type & " in clearing" &
                   Clear'Image & ".");
         Map_Builds (Clear) := Map_Builds (Clear) + 1;
         Supply := Supply - 1;
      else
         Put_Line ("Unable to place building: building supply is depleted!");
      end if;
   end Deploy_Building;

end Root.Faction;

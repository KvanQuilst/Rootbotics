-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . ALLIANCE (Body)                           --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the Automated Alliance faction   --
-- from Root: The Clockwork Expansion for use in The Rootbotics Assistant.   --
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

package body Root.Alliance is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
   begin
      Root.Faction.Put_Logo (Name, Logo, Logo_Width);
   end Put_Logo;

   procedure Put_State is
      procedure Forts_State is
         Fort : constant String := "|^|";
      begin
         Put ("             Forts:");
         for S in Clearing_Suit'Range loop
            Set_Style ((if Fort_Supply (S) > 0
                        then (case S is
                                 when Fox    => Fox_Color,
                                 when Mouse  => Mouse_Color,
                                 when Rabbit => Rabbit_Color)
                        else B_Black));
            Put (" " & Fort);
         end loop;
         Reset_Style;
         New_Line;
      end Forts_State;

      procedure Sympathy_State is
         Sympathy : constant String := "*";
      begin
         Put ("          Sympathy:");
         for I in reverse 1 .. SYMPATHY_MAX loop
            if I mod 2 = 0 then
               if Sympathy_Supply >= I then
                  Set_Style (Green);
               else
                  Set_Style (B_Black);
               end if;
               Put ("   " & Sympathy);
            end if;
         end loop;
         New_Line;
         Put ("                 ");
         for I in reverse 1 .. SYMPATHY_MAX loop
            if I mod 2 = 1 then
               if Sympathy_Supply >= I then
                  Set_Style (Green);
               else
                  Set_Style (B_Black);
               end if;
               if I /= SYMPATHY_MAX then
                  Put ("   " & Sympathy);
               end if;
            end if;
         end loop;
         Reset_Style;
         New_Line;
      end Sympathy_State;
   begin
      Put_Line ("    Warrior Supply:" & Warrior_Supply'Image);
      Put_Line ("          Officers:" & Officers'Image);
      Forts_State;
      Sympathy_State;
   end Put_State;

   procedure Prompt (Time : Phase := None) is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Forts, Rule, Curr_Order, Time);
   end Prompt;

   --------------------
   -- Alliance Setup --
   --------------------

   procedure Setup is null;

   ---------------
   -- Take Turn --
   ---------------
   procedure Birdsong is null;
   procedure Daylight is null;
   procedure Evening  is null;

   procedure Take_Turn (Order : Suit) is
   begin
      Curr_Order := Order;

      ----------------------
      -- Confirm Warriors --
      ----------------------
      declare
         Lost : constant Natural :=
            Check_Warriors (Prompt'Access, Warrior_Supply,
                            Map_Warriors, WARRIOR_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      -----------------------
      -- Confirm Buildings --
      -----------------------
      declare
         Lost : constant Natural :=
            Check_Buildings (Prompt'Access, Fort_Supply, Forts, FORTS_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      ------------------
      -- Confirm Rule --
      ------------------
      Check_Rule (Prompt'Access, Rule);

      --------------
      -- Birdsong --
      --------------
      Prompt (Birdsong);
      Birdsong;
      Continue;

      --------------
      -- Daylight --
      --------------
      Prompt (Daylight);
      Daylight;
      Continue;

      -------------
      -- Evening --
      -------------
      Prompt (Evening);
      Evening;
      Continue;

   end Take_Turn;

end Root.Alliance;

-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                           ROOT . DUCHY (Spec)                             --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Automated Alliance faction   --
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

with Root.Maps; use Root.Maps;

package body Root.Duchy is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
   begin
      Root.Faction.Put_Logo (Name, Logo, Logo_Width);
   end Put_Logo;

   procedure Put_State is null;

   procedure Put_Phase is
   begin
      Root.IO.Put_Phase (Curr_Phase, "");
   end Put_Phase;

   procedure Prompt is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Map_Buildings, Rule, Curr_Order, Put_Phase'Access,
                  Map_Tunnels);
   end Prompt;

   -----------------
   -- Duchy Setup --
   -----------------
   procedure Setup is
      C : Priority;
   begin
      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      New_Line;

      Put_Line ("Which clearing will the " & Name & " start in?");
      C := Get_Integer (1, 12);

      -- Starting Clearing --
      Map_Warriors (C) := 2;
      Warrior_Supply   := Warrior_Supply - 2;
      Map_Tunnels (C)  := True;
      Tunnel_Supply    := Tunnel_Supply - 1;

      for Neighbor of Clearings (C).Neighbors loop
         exit when Neighbor = 0;
         Map_Warriors (Neighbor) := 2;
         Warrior_Supply := Warrior_Supply - 2;
      end loop;

      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      New_Line;
      Put_Line ("Draw two cards. What is the suit of the first card?");
      Sway_Minister (Get_Suit_Opt);

      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      New_Line;
      Put_Line ("What is the suit of the second card?");
      Sway_Minister (Get_Suit_Opt);
      Put_Line ("Discard the cards.");
   end Setup;

   ----------------------
   -- Duchy Turn Logic --
   ----------------------
   procedure Take_Turn is
   begin
      Prompt;
      Continue;
   end Take_Turn;

   -----------
   -- Phase --
   -----------
   procedure Birdsong is null;

   procedure Daylight is null;

   procedure Evening is null;

   -------------
   -- Actions --
   -------------

   procedure Sway_Minister (S : Suit) is
   begin
      if S = Bird then
         for M in Minister'Range loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crown on the " &
                         To_String (Minister_Str (M)) & ".");
               exit;
            end if;
         end loop;
      else
         for M of Suit_Ministers (S) loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crown on the " &
                         To_String (Minister_Str (M)) & ".");
               exit;
            end if;
         end loop;
      end if;
      Continue;
   end Sway_Minister;

end Root.Duchy;

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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Root.Faction; use Root.Faction;
with Root.IO; use Root.IO;
package Root.Duchy is

   Faction_Color : constant Color := B_Magenta;
   Name : constant String := String_Style ("Drillbit Duchy", Faction_Color);

   procedure Setup;
   procedure Take_Turn;

private

   WARRIOR_MAX : constant Integer := 20;
   CROWN_MAX   : constant Integer := 9;
   TUNNEL_MAX  : constant Integer := 3;
   BUILD_MAX   : constant Integer := 3;

   type Action is (Craft, None);

   procedure Put_Phase with Inline;
   procedure Prompt    with Inline;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Warrior_Sypply : Integer range 0 .. WARRIOR_MAX := WARRIOR_MAX;
   Crown_Supply   : Integer range 0 .. CROWN_MAX   := CROWN_MAX;
   Citadel_Supply : Integer range 0 .. BUILD_MAX   := BUILD_MAX;
   Market_Supply  : Integer range 0 .. BUILD_MAX   := BUILD_MAX;
   Tunnel_Supply  : Integer range 0 .. TUNNEL_MAX  := TUNNEL_MAX;

   Map_Warriors   : Warrior_Arr   := (others => 0);
   Map_Buildings  : Building_Arr  := (others => 0);
   Map_Tunnels    : Token_Arr     := (others => False);
   Rule           : Rule_Arr      := (others => False);

   Curr_Order  : Suit;
   Curr_Phase  : Phase  := None;
   Curr_Action : Action := None;

   Logo_Width : constant := 24;
   Logo : Logo_Arr :=
      (Unbounded (String_Style ("                        ", Faction_Color)),
       Unbounded (String_Style ("   ____________         ", Faction_Color)),
       Unbounded (String_Style ("  /            \   \\/\ ", Faction_Color)),
       Unbounded (String_Style (" /     __        \_/ //|", Faction_Color)),
       Unbounded (String_Style ("|     /               /=", Faction_Color)),
       Unbounded (String_Style ("|           _________|  ", Faction_Color)),
       Unbounded (String_Style ("|           V       |   ", Faction_Color)),
       Unbounded (String_Style ("|                   |   ", Faction_Color)),
       Unbounded (String_Style (" \__________________\   ", Faction_Color)));

end Root.Duchy;

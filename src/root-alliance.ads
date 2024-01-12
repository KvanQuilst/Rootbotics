-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                         ROOT . ALLIANCE (Spec)                            --
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

package Root.Alliance is

   Faction_Color : constant Color := Green;
   Name : constant String := String_Style ("Automated Alliance", Green);

   procedure  Setup;
   procedure Take_Turn (Order : Suit);

private

   WARRIOR_MAX  : constant Integer := 10;
   SYMPATHY_MAX : constant Integer := 10;
   FORTS_MAX    : constant Integer := 1;

   subtype Fort is Building_Suit;

   procedure Prompt (Time : Phase := None) with Inline;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Warrior_Supply  : Integer range 0 .. WARRIOR_MAX := WARRIOR_MAX;
   Map_Warriors    : Warrior_Arr;
   Rule            : Rule_Arr;
   Officers        : Integer range 0 .. WARRIOR_MAX := 0;
   Sympathy_Supply : Integer range 0 .. SYMPATHY_MAX := SYMPATHY_MAX;
   Map_Sympathy    : array (Priority'Range) of Boolean;
   Forts           : Building_Arr;
   Fort_Supply     : Suit_Build_Supply := (FORTS_MAX, FORTS_MAX, FORTS_MAX);

   Curr_Order : Suit;
   Curr_Phase : Phase;

   Logo_Width : constant := 21;
   Logo : Logo_Arr :=
      (Unbounded (String_Style ("      _       _      ", Faction_Color)),
       Unbounded (String_Style ("     / \     / \     ", Faction_Color)),
       Unbounded (String_Style ("    |/ \|   |/ \|    ", Faction_Color)),
       Unbounded (String_Style ("    || ||   || ||    ", Faction_Color)),
       Unbounded (String_Style ("   _|| ||___|| ||_   ", Faction_Color)),
       Unbounded (String_Style ("  /  __       __  \  ", Faction_Color)),
       Unbounded (String_Style (" / /   /\   /   /\ \ ", Faction_Color)),
       Unbounded (String_Style ("| |   <  | |   <  | |", Faction_Color)),
       Unbounded (String_Style ("|  \ __\/   \ __\/  |", Faction_Color)),
       Unbounded (String_Style (" \  . .   |   . .  / ", Faction_Color)),
       Unbounded (String_Style ("  \_______________/  ", Faction_Color)));

end Root.Alliance;

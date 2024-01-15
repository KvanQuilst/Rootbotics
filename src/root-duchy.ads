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

   type Action is (Craft);

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Curr_Order  : Suit;
   Curr_Phase  : Phase;
   Curr_Action : Action;

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

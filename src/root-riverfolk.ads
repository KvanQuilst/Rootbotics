-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                         ROOT . RIVERFOLK (Spec)                           --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Riverfolk Robots faction     --
-- from Root: The Clockwork Expansion 2 for use in The Rootbotics Assistant. --
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

package Root.Riverfolk is

   Faction_Color : constant Color := Cyan;
   Name : constant String := String_Style ("Riverfolk Robots", Faction_Color);

   procedure Setup;
   procedure Take_Turn;

private

   type Action is (Craft);

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Curr_Order  : Suit;
   Curr_Phase  : Phase;
   Curr_Action : Action;

   Logo_Width : constant := 22;
   Logo : Logo_Arr :=
      (Unbounded (String_Style ("   \\\---------///   ", Faction_Color)),
       Unbounded (String_Style ("  / ___\     /___ \  ", Faction_Color)),
       Unbounded (String_Style (" | / /_\ ___ / /_\ | ", Faction_Color)),
       Unbounded (String_Style ("|  \___/ \ / \___/  |", Faction_Color)),
       Unbounded (String_Style ("|   . .  _|_  . .   |", Faction_Color)),
       Unbounded (String_Style ("|    .  /   \  .    |", Faction_Color)),
       Unbounded (String_Style (" \                 / ", Faction_Color)),
       Unbounded (String_Style ("  /\/___________\\\  ", Faction_Color)),
       Unbounded (String_Style (" / /             \\\ ", Faction_Color)));

end Root.Riverfolk;

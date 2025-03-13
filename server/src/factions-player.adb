-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                         FACTIONS . PLAYER (Body)                          --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the common faction-related        --
-- subroutines used throughout The Rootbotics Assistant.                     --
--                                                                           --
-- The Root Faction Assistant is free software: you can redistribute it      --
-- and/or modify it under the terms of the GNU General Public License as     --
-- published by the Free Software Foundation, either version 3 of the        --
-- License, or (at your option) any later version.                           --
--                                                                           --
-- The Root Faction Assistant is distributed in the hope that it will be     --
-- useful, but WITHOUT ANY WARRANTY; wihtout even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General  --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Assistant. If not, see                                --
-- <https://www.gnu.org/licenses/>.                                          --
-------------------------------------------------------------------------------
package body Factions.Player is

   function New_Player_Faction (Faction : Faction_Type) return Faction_Class is
      P : constant Faction_Class := new Player_Faction'(
         F_Type                               => Faction,
         S                                    => 1,
         Points | Items                       => <>,
         Crafted_Coffin_Makers                => <>
      );
   begin
      return P;
   end New_Player_Faction;

end Factions.Player;

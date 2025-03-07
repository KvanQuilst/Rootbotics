-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               GAME (Spec)                                 --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the centeral game logic for the  --
-- Root Faction Assisstant.                                                  --
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
with Root; use Root;
with Maps; use Maps;

package Game is

   ----------------
   -- Game Class --
   ----------------
   type Session (<>) is tagged private;

   -- Constructor --
   function New_Session (M_Type  : Map_Type;
                         M_Suits : Priority_Suits) return Session;

   function  Get_Map (Self : Session) return Map;

private

   type Session (M_Type  : Map_Type) is tagged record
      M : Map (M_Type);
      F : Faction_Type;
   end record;

end Game;

-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                                GAMES (Body)                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the centeral games logic for     --
-- the Root Faction Assistant.                                               --
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
package body Games is

   ------------------
   -- Game Methods --
   ----------------------------------------------------------------------------

   -- Constructor --
   function New_Game (M_Type      : Map_Type;
                      M_Suits     : Priority_Suits;
                      Num_Players : Seat) return Game is
      (M_Type      => M_Type,
       Num_Players => Num_Players,
       M           => New_Map (M_Type, M_Suits),
       Players     => [others => null],
       Factions_Set => <>);

   function Get_Map (Self : Game) return Map is
      (Self.M);

end Games;

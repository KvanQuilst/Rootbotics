-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Body)                            --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the common faction-related       --
-- subroutines used throughout The Rootbotics Assistant.                     --
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
package body Root.Faction is

   ---------------------
   -- Faction Methods --
   ---------------------
   function Get_Faction (Self : Faction) return Faction_Type is
      (Self.F_Type);

   function Score_Points (Self       : in out Faction;
                          Num_Points :        UInt8) return Boolean is
   begin
      Self.Points := Self.Points + Num_Points;
      return Self.Points >= 30;
   end Score_Points;

   -------------------------------
   -- Clockwork Faction Methods --
   -------------------------------
   function Get_Difficulty (Self : Clockwork_Faction) return Difficulty is
      (Self.Diff);

   procedure Take_Turn (Self  : in out Clockwork_Faction;
                        Order :        Suit) is null;

end Root.Faction;

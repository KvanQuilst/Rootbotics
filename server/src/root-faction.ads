-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Spec)                            --
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
package Root.Faction is

   type Faction_By_Seat is array (Seat'Range) of Faction_Type;

   -------------------
   -- Faction Class --
   -------------------
   type Faction (<>) is tagged private;

   -- Faction Methods --
   function Get_Faction (Self : Faction) return Faction_Type;

   procedure Take_Turn (Self : in out Faction) is null;

   function Score_Points (Self       : in out Faction;
                          Num_Points :        UInt8) return Boolean;

   -----------------------------
   -- Clockwork Faction Class --
   -----------------------------
   type Clockwork_Faction (<>) is new Faction with private;

   -- Clockwork Methods --
   function  Get_Difficulty (Self  : Clockwork_Faction) return Difficulty;

   procedure Take_Turn (Self  : in out Clockwork_Faction;
                        Order :        Suit) is null;

private

   -------------------
   -- Faction Class --
   -------------------
   type Faction (F_Type : Faction_Type) is tagged record
      S              : Seat;
      Points         : UInt8 range 0 .. 50 := 0;
      -- Dynamic Items Array --
   end record;

   type Clockwork_Faction is new Faction with record
      Diff           : Difficulty := Normal;
      Traits         : UInt8      := 0;
   end record;

end Root.Faction;

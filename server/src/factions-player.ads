-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                         FACTIONS . PLAYER (Spec)                          --
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
with Ada.Streams;

package Factions.Player is

   type Player_Faction is new Faction with private;

   -- Constructor --
   function New_Player_Faction (Faction : Faction_Type) return Faction_Class;

   -- Faction Methods --
   overriding
   procedure Setup     (Self : in out Player_Faction) is null;
   overriding
   procedure Take_Turn (Self : in out Player_Faction) is null;

   -- Serializatio Methods --
   overriding
   procedure Send (Self : Player_Faction) is null;

   overriding
   procedure Receive (
      Self   : in out Player_Faction;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class
   ) is null;

private

   type Player_Faction is new Faction with
      record
         Crafted_Coffin_Makers : Boolean := False;
      end record;

end Factions.Player;

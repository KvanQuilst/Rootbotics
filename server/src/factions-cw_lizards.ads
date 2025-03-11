-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                       FACTIONS . CW_LIZARDS (Spec)                        --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the Logical Lizards faction.      --
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
with Messages; use Messages;

package Factions.CW_Lizards is

   type Logical_Lizards is new Faction with private;

   -- Faction Methods --
   overriding
   procedure Setup     (Self : in out Logical_Lizards) is null;
   overriding
   procedure Take_Turn (Self : in out Logical_Lizards) is null;

   -- Serialization Methods --
   overriding
   procedure Send (Self : Logical_Lizards) is null;

   overriding
   procedure Receive (
      Self   : in out Logical_Lizards;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class
   ) is null;

private

   type Logical_Lizards is new Faction (Lizards) with
      record
         null;
      end record;

end Factions.CW_Lizards;

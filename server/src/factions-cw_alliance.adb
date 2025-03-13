-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                       FACTIONS . CW_ALLIANCE (Body)                       --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the Automated Alliance faction.  --
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
with Ada.Streams; use Ada.Streams;

with Messages; use Messages;
with Server;

package body Factions.CW_Alliance is

   function New_Automated_Alliance return Faction_Class is
      AA : constant Faction_Class := new Automated_Alliance'(
         F_Type                               => Alliance,
         S                                    => 1,
         Points | Items                       => <>,
         Warrior_Supply | Map_Warriors        => <>,
         Sympathy_Supply | Sympathetic_Clears => <>,
         Base_Supply | Base_Clears            => <>
      );
   begin
      return AA;
   end New_Automated_Alliance;

   -------------------------
   -- Serializing Methods --
   ----------------------------------------------------------------------------
   overriding
   procedure Send (Self : Automated_Alliance) is
      Payload : constant Automated_Alliance_Msg := (
         Base   => (Faction  => Alliance,
                    S        => Self.S,
                    Points   => Self.Points)
      );
   begin
      Server.Send (Payload);
   end Send;

   overriding
   procedure Receive (Self   : in out Automated_Alliance;
                      Stream : not null access Root_Stream_Type'Class) is null;

end Factions.CW_Alliance;

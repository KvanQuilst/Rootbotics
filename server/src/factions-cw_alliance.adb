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

package body Factions.CW_Alliance is

   function New_Faction return Automated_Alliance is
      (F_Type                               => Alliance,
       S                                    => 1,
       Points | Items                       => <>,
       Warrior_Supply | Map_Warriors        => <>,
       Sympathy_Supply | Sympathetic_Clears => <>,
       Base_Supply | Base_Clears            => <>
      );

   -------------------------
   -- Serializing Methods --
   ----------------------------------------------------------------------------
   overriding
   procedure Send (Self   : Automated_Alliance;
                   Stream : not null access Root_Stream_Type'Class) is
      Payload : constant Automated_Alliance_Msg := (
         Header => (Length   => Automated_Alliance_Msg'Size / 8,
                    Msg_Type => Messages.Faction),
         Base   => (Faction  => Alliance,
                    S        => Self.S,
                    Points   => Self.Points)
      );
   begin
      Automated_Alliance_Msg'Output (Stream, Payload);
   end Send;

   overriding
   procedure Receive (Self   : in out Automated_Alliance;
                      Stream : not null access Root_Stream_Type'Class) is
      Payload : constant Automated_Alliance_Msg :=
         Automated_Alliance_Msg'Input (Stream);
   begin
      -- Message Length Check --
      if Payload.Header.Length /= (Automated_Alliance_Msg'Size / 8) then
         -- TODO: Non-terminating error msg --
         return; -- No update --
      end if;

      -- Message Type Check --
      if Payload.Header.Msg_Type /= Messages.Faction or else
         Payload.Base.Faction    /= Alliance         or else
         Payload.Base.S          /= Self.S
      then
         -- TODO: Non-terminating error msg --
         return; -- No update --
      end if;

      Self.Points := Payload.Base.Points;
   end Receive;

end Factions.CW_Alliance;

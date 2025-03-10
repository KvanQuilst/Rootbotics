-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                             FACTIONS (Body)                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the common faction-related       --
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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with Games; use Games;
with Messages; use Messages;

with Factions.CW_Alliance; use Factions.CW_Alliance;
with Factions.CW_Lizards; use Factions.CW_Lizards;
with Factions.CW_Duchy; use Factions.CW_Duchy;

package body Factions is

   ----------------------
   -- Message Handling --
   ----------------------------------------------------------------------------
   procedure Receive (Stream : not null access Root_Stream_Type'Class;
                      Length : UInt8) is
   begin
      if Length < Faction_Msg_Len then
         --  TODO: Non-terminating error msg
         Put_Line ("> ERROR: FACTIONS . RECEIVE: "
                 & "Faction message length too short:" & Length'Image);
         return;
      end if;

      declare
         Payload : constant Faction_Msg := Faction_Msg'Input (Stream);
      begin
         case Payload.Faction is
            when others =>
               Put_Line ("> ERROR: FACTION . RECEIVE: "
                       & "Faction message unimplemented!");
         end case;
      end;
   exception
      when Constraint_Error =>
         --  TODO: Non-terminating error msg
         Put_Line ("> ERROR: FACTIONS . RECEIVE: "
                 & "Invalid message.");
      when E : others =>
         Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         raise;
   end Receive;

   ---------------------
   -- Faction Methods --
   ----------------------------------------------------------------------------
   function Get_Faction (Self : Faction) return Faction_Type is
      (Self.F_Type);

   function Score_Points (Self       : in out Faction;
                          Num_Points :        UInt8) return Boolean is
   begin
      Self.Points := Self.Points + Num_Points;
      return Self.Points >= 30;
   end Score_Points;

end Factions;

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

with Logs; use Logs;
with Messages; use Messages;

with Factions.CW_Alliance;
with Factions.Player;

package body Factions is

   ----------------------
   -- Message Handling --
   ----------------------------------------------------------------------------
   procedure Receive (Stream : not null access Root_Stream_Type'Class;
                      Length : UInt8) is
   begin
      if Length < Faction_Msg_Len then
         Put_Msg (Warning, "FACTIONS . RECEIVE: "
                & "Faction message length too short:" & Length'Image);
         return;
      end if;

      declare
         Payload : constant Faction_Msg := Faction_Msg'Input (Stream);
      begin
         case Payload.Faction is
            when others =>
               Put_Msg (Warning, "FACTIONS . RECEIVE: "
                      & "Faction message unimplemented!");
         end case;
      end;
   exception
      when Constraint_Error =>
         Put_Msg (Warning, "FACTIONS . RECEIVE: "
                & "Invalid message.");
      when E : others =>
         Put_Msg (Error, Exception_Name (E) & ": " & Exception_Message (E));
         raise;
   end Receive;

   ---------------------
   -- Faction Methods --
   ----------------------------------------------------------------------------
   -- Constructor --
   function New_Faction (F_Type    : Faction_Type;
                         Clockwork : Boolean) return Faction_Class is
      (if Clockwork
       then
         (case F_Type is
            when Alliance =>
               CW_Alliance.New_Automated_Alliance,
            when others =>
               null
         )
      else
         Player.New_Player_Faction (F_Type)
      );

   function Get_Faction (Self : Faction) return Faction_Type is
      (Self.F_Type);

   function Score_Points (Self       : in out Faction;
                          Num_Points :        UInt8) return Boolean is
   begin
      Self.Points := Self.Points + Num_Points;
      return Self.Points >= 30;
   end Score_Points;

   procedure Add_Item (Self : in out Faction;
                       I    :        Item) is
   begin
      Self.Items (I) := @ + 1;
   end Add_Item;

   function Remove_Item (Self : in out Faction;
                         I    :        Item) return Boolean is
   begin
      if Self.Items (I) = 0 then
         Put_Msg (Error, "FACTIONS . REMOVE_ITEM: "
                & "There are no " & I'Image & "s in the supply to remove.");
         return False;
      end if;
      Self.Items (I) := @ - 1;
      return True;
   end Remove_Item;

end Factions;

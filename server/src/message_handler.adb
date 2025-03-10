-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                          MESSAGE_HANDLER (Body)                           --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation for the message handling aspect     --
-- of the Root Faction Assistant. This is the package which handles          --
-- interactions with clients.                                                --
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

with Messages; use Messages;
with Types; use Types;

package body Message_Handler is

   procedure Receive (Stream : not null access Root_Stream_Type'Class) is
      --  Payload : constant Msg_Header := Msg_Header'Input (Stream);
      Length       : constant UInt8 := UInt8'Input (Stream);
      Msg_Type_Val : constant UInt8 := UInt8'Input (Stream);
      Msg_Type     : Message_Type;
   begin
      if Length <= 2 then
         --  TODO: Non-terminating error msg
         return;
      end if;

      Msg_Type := Message_Type'Val (Msg_Type_Val);

      case Msg_Type is
         when others =>
            Put_Line ("> TODO: Unimplemented message type!");
      end case;

   exception
      when Constraint_Error =>
         --  TODO: Non-terminating error msg
         Put_Line ("> ERROR: Unrecognized message type!");
      when E : others =>
         Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         raise;
   end Receive;

end Message_Handler;

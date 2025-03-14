-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               SERVER (Body)                               --
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
with Ada.Text_IO; use Ada.Text_IO;

with Factions;
with Types; use Types;

package body Server is

   procedure Initialize (Port : Port_Type := 5864) is
      Address : Sock_Addr_Type;
   begin
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := Port;
      Create_Socket (Server);

      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

      Bind_Socket (Server, Address);

      Put_Line ("> Waiting for client to connect (Port :=" & Port'Image & ")");
      Listen_Socket (Server);
      Accept_Socket (Server, Socket, Address);

      Channel := Stream (Socket);
   end Initialize;

   procedure Finalize is
   begin
      Close_Socket (Server);
      Close_Socket (Socket);
   end Finalize;

   procedure Receive is
      --  Payload : constant Msg_Header := Msg_Header'Input (Stream);
      Length       : constant UInt8 := UInt8'Input (Channel);
      Msg_Type_Val : constant UInt8 := UInt8'Input (Channel);
      Msg_Type     : Message_Type;
   begin
      if Length <= Msg_Header_Len then
         --  TODO: Non-terminating error msg
         Put_Line ("> ERROR: SERVER . RECEIVE: "
                 & "Message length too short:" & Length'Image);
         return;
      end if;

      Msg_Type := Message_Type'Val (Msg_Type_Val);

      Put_Line ("> DEBUG: " & Msg_Type'Image);

      case Msg_Type is
         when Faction =>
            Factions.Receive (Channel, Length - 2);
         when others =>
            Put_Line ("> ERROR: SERVER . RECEIVE: "
                    & "Unimplemented message type!");
      end case;

   exception
      when Constraint_Error =>
         --  TODO: Non-terminating error msg
         Put_Line ("> ERROR: SERVER . RECEIVE: "
                 & "Unrecognized message type:" & Msg_Type_Val'Image);
      when E : others =>
         Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
         raise;
   end Receive;

   ----------------------
   -- Send Subprograms --
   ----------------------------------------------------------------------------

   function Get_Header (Bits : UInt8;
                        Msg_Type : Message_Type) return Msg_Header is
      (Length   => (Msg_Header'Size + Bits) / 8,
       Msg_Type => Msg_Type);

   -- Faction Messages --
   procedure Send (Payload : Automated_Alliance_Msg) is
   begin
      Msg_Header'Output (Channel,
                         Get_Header (Automated_Alliance_Msg'Size, Faction));
      Automated_Alliance_Msg'Output (Channel, Payload);
   end Send;

end Server;

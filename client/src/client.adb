-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                              CLIENT (Body)                                --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the terminal client for managing Leder Games' Root:    --
-- Clockwork Expansion factions.                                             --
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
with Ada.Text_IO; use Ada.Text_IO;

with Game;
with Types; use Types;

package body Client is

   procedure Initialize (Port : Port_Type := 5864) is
      Address : Sock_Addr_Type;

      function Try_Connect return Boolean is
      begin
         Connect_Socket (Client, Address);
         Channel := Stream (Client);
         return True;
      exception when Socket_Error =>
         Put_Line ("Server unavailable... trying again in 5 seconds.");
         return False;
      end Try_Connect;
   begin
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := Port;
      Create_Socket (Client);

      Set_Socket_Option (Client, Socket_Level, (Reuse_Address, True));

      loop
         if Try_Connect then
            Put_Line ("Connected to server!");
            return;
         end if;
         delay 5.0;
      end loop;
   end Initialize;

   procedure Finalize is
   begin
      Close_Socket (Client);
   end Finalize;

   function Exiting return Boolean is
      (Client_Exit);

   procedure Receive is
      Length   : constant UInt8        := UInt8'Input (Channel);
      Msg_Type : constant Message_Type := Message_Type'Input (Channel);

      pragma Assert (Length >= Msg_Header_Len,
                     "ERROR: Invalid message length from server!");
   begin
      case Msg_Type is
         when Request_Create_Game => Game.Create_Game;
         when Request_Map_Clears  => Game.Map_Clears;
         when others =>
            Put_Line ("ERROR: CLIENT . RECEIVE: "
                    & "Unimplemented message type: " & Msg_Type'Image);
      end case;
   end Receive;

   ----------------------
   -- Send Subprograms --
   ----------------------------------------------------------------------------
   function Get_Header (Length    : UInt8;
                        Msg_Type : Message_Type) return Msg_Header is
      (Length   => Msg_Header_Len + Length,
       Msg_Type => Msg_Type);

   procedure Send (Payload : Create_Game_Msg) is
   begin
      Msg_Header'Output (Channel,
                         Get_Header (Create_Game_Msg_Len, Create_Game));
      Create_Game_Msg'Output (Channel, Payload);
   end Send;

   procedure Send (Payload : Map_Clears_Msg) is
   begin
      Msg_Header'Output (Channel,
                         Get_Header (Map_Clears_Msg_Len, Map_Clears));
      Map_Clears_Msg'Output (Channel, Payload);
   end Send;

end Client;

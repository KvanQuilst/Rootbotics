-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                                ROOTBOTICS                                 --
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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Sockets; use GNAT.Sockets;

with Messages; use Messages;
with Root; use Root;
with Types; use Types;

procedure Rootbotics is
   VERSION : constant String := "v0.3-dev";

   -- Networking --
   Address : Sock_Addr_Type;
   Socket  : Socket_Type;
   Channel : Stream_Access;
begin
   Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
   Address.Port := 5876;
   Create_Socket (Socket);

   Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));

   Connect_Socket (Socket, Address);
   Channel := Stream (Socket);

   declare
      --  Msg : Automated_Alliance_Msg := Automated_Alliance_Msg'Input (Channel);
      Len : constant UInt8 := UInt8'Input (Channel);
      Msg : UInt8;
   begin
      Put_Line ("Length:" & Len'Image);
      for I in 0 .. (Len - 1) loop
         Msg := UInt8'Input (Channel);
         Put_Line ("Byte" & I'Image & ":" & Msg'Image);
      end loop;
      --  Put_Line ("Faction:" & Msg.Base.Faction'Image);
      --  Put_Line ("Seat:" & Msg.Base.S'Image);
      --  Put_Line ("Points:" & Msg.Base.Points'Image);
   end;

   declare
      Header  : constant Msg_Header :=
         (Length   => 4,
          Msg_Type => Faction);
      Payload : constant Faction_Msg :=
         (Faction => Alliance,
          S       => 1,
          Points  => 10);
   begin
     Msg_Header'Output (Channel, Header);
     --  Faction_Msg'Output (Channel, Payload);
     UInt4'Output (Channel, 1);
     UInt4'Output (Channel, 1);
     UInt8'Output (Channel, 40);
   end;

   Close_Socket (Socket);

exception when E : others =>
   Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
end Rootbotics;

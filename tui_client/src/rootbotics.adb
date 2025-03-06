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

procedure Rootbotics is
   VERSION : constant String := "v0.3-dev";

   type UInt8 is mod 2**8;

   type T is (Cats, Mice);

   type Msg (Ty : T) is record
      case Ty is
         when Cats =>
            B : UInt8;
            C : UInt8;
            D : UInt8;
         when Mice =>
            M : UInt8;
            N : UInt8;
            O : UInt8;
      end case;
   end record with Pack;

   M : constant Msg := (Cats, 0, 0, 16#80#);
   N : constant UInt8 := 0;
   O : constant UInt8 := 16#80#;

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

   UInt8'Output (Channel, O);
   UInt8'Output (Channel, N);
   UInt8'Output (Channel, N);
   UInt8'Output (Channel, N);

   Close_Socket (Socket);

exception when E : others =>
   Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
end Rootbotics;

-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                            ROOTBOTICS SERVER                              --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the main server loop for managing Leder Games' Root:   --
-- Clockwork Expansion bots.                                                 --
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
with GNAT.Sockets; use GNAT.Sockets;

with Factions; use Factions;
with Factions.CW_Alliance;
with Servers;

procedure Rootbotics_Server is
   VERSION : constant String := "v0.3-dev";

   F : constant Faction'Class := Factions.CW_Alliance.New_Faction;

   -- Networking --
   Address         : Sock_Addr_Type;
   Server, Socket  : Socket_Type;
   Channel         : Stream_Access;

begin
   -- Setup Server --
   Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
   Address.Port := 5876;
   Create_Socket (Server);

   Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

   Bind_Socket (Server, Address);
   Listen_Socket (Server);
   Accept_Socket (Server, Socket, Address);
   Channel := Stream (Socket);

   --  Faction'Output (Channel, F);
   F.Send (Channel);

   Servers.Receive (Channel);

   Close_Socket (Server);
   Close_Socket (Socket);

exception when E : others =>
   Put_Line (Exception_Name (E) & ": " & Exception_Message (E));
end Rootbotics_Server;

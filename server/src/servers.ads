-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                              SERVERS (Spec)                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the message handling aspect      --
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
with GNAT.Sockets; use GNAT.Sockets;

with Ada.Streams;

package Servers is

   procedure Receive
      (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   ------------------
   -- Server Class --
   ------------------
   type Server is tagged private;

private

   type Server is tagged
      record
         Address : Sock_Addr_Type;
         Server  : Socket_Type;
      end record;

end Servers;

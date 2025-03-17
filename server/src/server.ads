-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               SERVER (Spec)                               --
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
with Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

with Messages; use Messages;

package Server is

   procedure Initialize (Port : Port_Type := 5864);
   procedure Finalize;

   ----------------------------
   -- Serializable Interface --
   ----------------------------
   type Serializable is interface;
   procedure Send (Self : Serializable) is abstract;
   procedure Receive (
      Self   : in out Serializable;
      Stream : not null access Ada.Streams.Root_Stream_Type'Class
   ) is abstract;

   procedure Receive (Expect : Message_Type);

   ----------------------
   -- Send Subprograms --
   ----------------------

   procedure Send (Req_Type : Messages.Request_Type);

   -- Faction Messages --
   procedure Send (Payload : Automated_Alliance_Msg);

private

   Server  : Socket_Type;
   Socket  : Socket_Type;
   Channel : Stream_Access;

end Server;

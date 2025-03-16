-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                              CLIENT (Spec)                                --
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
with GNAT.Sockets; use GNAT.Sockets;

with Messages; use Messages;

package Client is

   procedure Initialize (Port : Port_Type := 5864);
   procedure Finalize;

   function Exiting return Boolean;

   procedure Receive;

   procedure Send (Payload : Create_Game_Msg);
   procedure Send (Payload : Map_Clears_Msg);

private

   Client  : Socket_Type;
   Channel : Stream_Access;

   Client_Exit : Boolean := False;

end Client;

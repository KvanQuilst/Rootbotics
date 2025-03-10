-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                                MESSAGES                                   --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the protocol the server uses for communicating with    --
-- clients.                                                                  --
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
with Root; use Root;
with Types; use Types;

package Messages is

   ----------------------------------------------------------------------------
   -- NOTE: When adding or changing message types, need to ensure the message
   --       record is always a multiple of 8-bytes in size. Otherwise, the
   --       *_Len constant will break;
   ----------------------------------------------------------------------------

   type Message_Type is (
      Game,
      Faction
   ) with Size => 8;

   for Message_Type use (
      Game    => 0,
      Faction => 1
   );

   type Msg_Header is record
      Length   : UInt8;
      Msg_Type : Message_Type;
   end record;

   for Msg_Header use record
      Length   at 0 range 0 .. 7;
      Msg_Type at 1 range 0 .. 7;
   end record;
   Msg_Header_Len : constant UInt8 := (Msg_Header'Size / 8);

   ----------------------
   -- Faction Messages --
   ----------------------
   type Faction_Msg is record
      Faction : Faction_Type;
      S       : Seat;
      Points  : Point;
   end record;

   for Faction_Msg use record
      Faction at 0 range 0 .. 3;
      S       at 0 range 4 .. 7;
      Points  at 1 range 0 .. 7;
   end record;
   Faction_Msg_Len : constant UInt8 := (Faction_Msg'Size / 8);

   -- Automated Alliance --
   type Automated_Alliance_Msg is record
      Header : Msg_Header;
      Base   : Faction_Msg;
   end record;

   for Automated_Alliance_Msg use record
      Header at 0 range 0 .. 15;
      Base   at 2 range 0 .. 15;
   end record;

end Messages;

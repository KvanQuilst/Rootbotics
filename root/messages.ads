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
   --  NOTE: When adding or changing message types, need to ensure the message
   --        record is always a multiple of 8-bytes in size. Otherwise, the
   --        *_Len constant will break;
   ----------------------------------------------------------------------------
   type Message_Type is (
      Game,
      Faction,
      Map,
      Error,
      Request,
      Create_Game,
      Map_Clears,
      Battle
   ) with Size => 8;

   for Message_Type use (
      Game                => 0,
      Faction             => 1,
      Map                 => 2,
      Error               => 3,
      Request             => 4,

      -- Client Responses --
      Create_Game         => 5, -- Client --
      Map_Clears          => 6, -- Client --
      Battle              => 7  -- Client --
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

   --------------
   -- Requests --
   ----------------------------------------------------------------------------
   type Request_Type is (
      Create_Game,
      Map_Clears,
      Battle
   ) with Size => 8;

   for Request_Type use (
      Create_Game => 0,
      Map_Clears  => 1,
      Battle      => 2
   );

   type Request_Msg is record
      Req_Type : Request_Type;
   end record;

   for Request_Msg use record
      Req_Type at 0 range 0 .. 7;
   end record;
   Request_Msg_Len : constant UInt8 := (Request_Msg'Size / 8);

   type Request_Map_Clears_Msg is record
      Base : Request_Msg;
      Map  : Map_Type;
   end record;

   for Request_Map_Clears_Msg use record
      Base at 0 range 0 .. 7;
      Map  at 1 range 0 .. 7;
   end record;
   Request_Map_Clears_Msg_Len : constant UInt8 :=
      (Request_Map_Clears_Msg'Size / 8);

   ------------------
   -- Map Messages --
   ----------------------------------------------------------------------------
   type Map_Clearings is (
      Balanced,
      Random
   ) with Size => 1;

   -- Client Message --
   type Map_Clears_Msg is record
      Clearing_Suits : Clearing_Suit_By_Priority;
   end record;

   for Map_Clears_Msg use record
      Clearing_Suits at 0 range 0 .. (Priority'Last * Suit'Size) - 1;
   end record;
   Map_Clears_Msg_Len : constant UInt8 := (Map_Clears_Msg'Size / 8);

   -- Server Message --
   type Map_Msg is record
      null;
   end record;

   -------------------
   -- Game Messages --
   ----------------------------------------------------------------------------

   -- Client Message --
   type Create_Game_Msg is record
      AdSet        : Boolean;
      Deck         : Deck_Type;
      Map          : Map_Type;
      Clearing_Set : Map_Clearings;
      Padding      : Boolean;
      Num_Players  : UInt4;
   end record;

   for Create_Game_Msg use record
      AdSet        at 0 range 0 .. 0;
      Deck         at 0 range 1 .. 2;
      Map          at 0 range 3 .. 5;
      Clearing_Set at 0 range 6 .. 6;
      Padding      at 0 range 7 .. 7;

      Num_Players  at 1 range 0 .. 7;
   end record;
   Create_Game_Msg_Len : constant UInt8 := (Create_Game_Msg'Size / 8);

   ----------------------
   -- Faction Messages --
   ----------------------------------------------------------------------------

   -- Server Message --
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

   -- Automated Alliance | Server Message --
   type Automated_Alliance_Msg is record
      Base   : Faction_Msg;
   end record;

   for Automated_Alliance_Msg use record
      Base   at 0 range 0 .. 15;
   end record;
   Automated_Alliance_Msg_Len : constant UInt8 :=
      (Automated_Alliance_Msg'Size / 8);

end Messages;

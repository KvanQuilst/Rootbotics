-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               GAMES (Spec)                                --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the centeral game logic for the  --
-- Root Faction Assisstant.                                                  --
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

with Factions; use Factions;
with Messages;
with Root; use Root;
with Types; use Types;

package Games is

   ----------------------
   -- Message Handling --
   ----------------------
   function Receive (
      Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Length   : UInt8
   ) return Boolean;

   ----------------
   -- Game Class --
   ----------------
   type Game (<>) is tagged private;
   type Game_Access is access Game;

   -- Constructor --
   procedure New_Game (AdSet        : Boolean;
                       Deck         : Deck_Type;
                       M_Type       : Map_Type;
                       Clearing_Set : Messages.Map_Clearings;
                       Num_Players  : Seat);

   function Factions_Set   (Self : Game) return Boolean;

   -- For standard setup and clockwork factions always --
   function Set_Faction (Self      : in out Game;
                         S         :        Seat;
                         Clockwork :        Boolean;
                         Faction   :        Faction_Type) return Boolean;

   -- Only for player factions --
   function Set_Adset_Faction (Self    : in out Game;
                               Faction :        Faction_Type) return Boolean;

   function Get_Current_Game return Game_Access;

private

   type Faction_By_Seat is array (Seat range <>) of Faction_Class;

   type Game (AdSet       : Boolean;
              Deck        : Deck_Type;
              Num_Players : Seat) is tagged
      record
         Players : Faction_By_Seat (1 .. Num_Players);
         -- Configure post-Initialization, pre-Start --
         Factions_Set : Boolean := False;
      end record;

   Current_Game : Game_Access := null;

end Games;

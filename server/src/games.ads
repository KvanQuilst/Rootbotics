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
with Maps; use Maps;
with Messages;
with Root; use Root;
with Types; use Types;

package Games is

   ----------------------
   -- Message Handling --
   ----------------------
   procedure Receive (
      Stream   : not null access Ada.Streams.Root_Stream_Type'Class;
      Length   : UInt8
   );

   ----------------
   -- Game Class --
   ----------------
   type Game (<>) is tagged private;
   type Game_Access is access Game;

   -- Constructor --
   function New_Game (AdSet       : Boolean;
                      Deck        : Deck_Type;
                      M_Type      : Map_Type;
                      Num_Players : Seat) return Game_Access;

   function  Get_Map (Self : Game) return Map;

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
              M_Type      : Map_Type;
              Num_Players : Seat) is tagged
      record
         M       : Map (M_Type);
         Players : Faction_By_Seat (1 .. Num_Players);
         Phase   : Messages.Game_Phase;
         -- Configure post-Initialization, pre-Start --
         Map_Set      : Boolean := False;
         Factions_Set : Boolean := False;
      end record;

   Curr_Game : Game_Access := null;

end Games;

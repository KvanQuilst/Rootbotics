-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                                GAMES (Body)                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the centeral games logic for     --
-- the Root Faction Assistant.                                               --
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
with Ada.Text_IO; use Ada.Text_IO;

package body Games is

   ------------------
   -- Game Methods --
   ----------------------------------------------------------------------------
   -- Constructor --
   function New_Game (Adset       : Boolean;
                      M_Type      : Map_Type;
                      M_Suits     : Priority_Suits;
                      Num_Players : Seat) return Game is
      (Adset       => Adset,
       M_Type      => M_Type,
       Num_Players => Num_Players,
       M           => New_Map (M_Type, M_Suits),
       Players     => [others => null],
       Factions_Set => <>);

   function Get_Map (Self : Game) return Map is
      (Self.M);

   -- Phase: Pick Factions

   --  TODO: Consider boolean function?
   procedure Set_Faction (Self      : in out Game;
                          S         :        Seat;
                          Clockwork :        Boolean;
                          Faction   :        Faction_Type) is
   begin
      if not Clockwork and then Self.Adset then
         --  TODO: Error sent to controlling client
         Put_Line ("> ERROR: GAME . SET_FACTION: "
                 & "Cannot specify seat for player faction with Adset.");
         return;
      end if;

      if S > Self.Num_Players then
         --  TODO: Error sent to controlling client
         Put_Line ("> ERROR: GAME . SET_FACTION: "
                 & "Seat specified exceeds number of players playing.");
         return;
      end if;

      if not Self.Players (S) = null then
         Put_Line ("> INFO: Re-assigning seat" & S'Image
                 & "to " & Faction'Image);
      end if;
      Self.Players (S) := New_Faction (Faction);
   end Set_Faction;

   --  TODO: Considering boolean function?
   procedure Set_Adset_Faction (Self    : in out Game;
                                Faction :        Faction_Type) is
   begin
      if not Self.Adset then
         --  TODO: Error sent to controlling client
         Put_Line ("> ERROR: GAME . SET_ADSET_FACTION: " &
                   "Cannot set AdSet faction in non-AdSet game.");
         return;
      end if;

      for S in Seat'First .. Self.Num_Players loop
         if Self.Players (S) = null then
            --  TODO: Server INFO message
            Put_Line ("> INFO: Player" & S'Image
                    & " is playing the " & Faction'Image);
            Self.Players (S) := New_Faction (Faction);
         end if;
      end loop;

      --  TODO: Error sent to client
      Put_Line ("> ERROR: GAME . SET_ADSET_FACTION: "
              & " All players already assigned a faction.");
   end Set_Adset_Faction;

end Games;

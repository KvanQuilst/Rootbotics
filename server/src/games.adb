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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with Logs; use Logs;
with Maps;

package body Games is

   ----------------------
   -- Message Handling --
   ----------------------------------------------------------------------------
   procedure Receive (Stream   : not null access Root_Stream_Type'Class;
                      Length   : UInt8) is
   begin
      if Length < Messages.Create_Game_Msg_Len then
         Put_Msg (Warning, "GAME . RECEIVE: "
            & "Game creation message length too short:" & Length'Image);
         return;
      end if;

      declare
         Payload : constant Messages.Create_Game_Msg :=
            Messages.Create_Game_Msg'Input (Stream);
      begin
         New_Game (Payload.AdSet,
                   Payload.Deck,
                   Payload.Map,
                   Payload.Clearing_Set,
                   Payload.Num_Players);
      exception
         when Constraint_Error =>
            Put_Msg (Warning, "GAMES . RECEIVE: "
                   & "Create_Game message invalid!");
         when E : others =>
            Put_Msg (Error, Exception_Name (E) & ": " & Exception_Message (E));
            raise;
      end;
   end Receive;

   ------------------
   -- Game Methods --
   ----------------------------------------------------------------------------
   -- Constructor --
   procedure New_Game (AdSet        : Boolean;
                       Deck         : Deck_Type;
                       M_Type       : Map_Type;
                       Clearing_Set : Messages.Map_Clearings;
                       Num_Players  : Seat) is
   begin
      Maps.New_Map (M_Type, Clearing_Set);
      Current_Game := new Game'(AdSet       => AdSet,
                                Deck        => Deck,
                                Num_Players => Num_Players,
                                Players     => [others => null],
                                Factions_Set => <>);
   end New_Game;

   function Factions_Set (Self : Game) return Boolean is
      (Self.Factions_Set);

   function Set_Faction (Self      : in out Game;
                         S         :        Seat;
                         Clockwork :        Boolean;
                         Faction   :        Faction_Type) return Boolean is
   begin
      if not Clockwork and then Self.AdSet then
         --  TODO: Error sent to controlling client
         Put_Line ("> ERROR: GAME . SET_FACTION: "
                 & "Cannot specify seat for player faction with Adset.");
         return False;
      end if;

      if S > Self.Num_Players then
         --  TODO: Error sent to controlling client
         Put_Line ("> ERROR: GAME . SET_FACTION: "
                 & "Seat specified exceeds number of players playing.");
         return False;
      end if;

      if not (Self.Players (S) = null) then
         Put_Line ("> INFO: Re-assigning seat" & S'Image
                 & "to " & Faction'Image);
      end if;
      Self.Players (S) := New_Faction (Faction, Clockwork);
      return True;
   end Set_Faction;

   function Set_Adset_Faction (Self    : in out Game;
                               Faction :        Faction_Type) return Boolean is
   begin
      if not Self.AdSet then
         --  TODO: Error sent to controlling client
         Put_Line ("> ERROR: GAME . SET_ADSET_FACTION: " &
                   "Cannot set AdSet faction in non-AdSet game.");
         return False;
      end if;

      for S in Seat'First .. Self.Num_Players loop
         if Self.Players (S) = null then
            Put_Msg (Info, "Player" & S'Image
                   & " is playing the " & Faction'Image);
            Self.Players (S) := New_Faction (Faction, False);
            return True;
         end if;
      end loop;

      --  TODO: Error sent to client
      Put_Line ("> ERROR: GAME . SET_ADSET_FACTION: "
              & " All players already assigned a faction.");
      return False;
   end Set_Adset_Faction;

   function Get_Current_Game return Game_Access is
      (Current_Game);

end Games;

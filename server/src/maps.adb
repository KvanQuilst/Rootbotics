-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                                 MAPS (Body)                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation for the map related components of   --
-- The Root Faction Assistant.                                               --
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

with Logs; use Logs;
with Messages; use Messages;

package body Maps is

   function Receive (Stream : not null access Root_Stream_Type'Class;
                     Length : UInt8) return Boolean is
   begin
      if Length < Messages.Map_Clears_Msg_Len then
         Put_Msg (Warning, "MAP . RECEIVE: "
            & "Game creation message length too short:" & Length'Image);
         return False;
      end if;

      if Current_Map /= null then
         Put_Msg (Warning, "MAP . RECEIVE: "
                & "Attempting to create game when one already created!");
         return False;
      end if;

      declare
         Payload : constant Messages.Map_Clears_Msg :=
            Messages.Map_Clears_Msg'Input (Stream);
      begin
         return Current_Map.Set_Clearing_Suits (Payload.Clearing_Suits);
      exception
         when Constraint_Error =>
            Put_Msg (Warning, "MAP . RECEIVE: "
                   & "Create_Game message invalid!");
            return False;
         when E : others =>
            Put_Msg (Error, Exception_Name (E) & ": " & Exception_Message (E));
            raise;
      end;
   end Receive;

   -----------------
   -- Map Methods --
   -----------------

   -- Constructors --
   procedure New_Map (M_Type       : Map_Type;
                      Clearing_Set : Messages.Map_Clearings) is
   begin
      Current_Map := new Map'(
         M_Type      => M_Type,
         Item_Supply => <>,
         Clears_Set  => (Clearing_Set = Messages.Balanced),
         Clearings   => (case M_Type is
                            when Fall     => Fall_Clearings,
                            when Winter   => Winter_Clearings,
                            when Lake     => Lake_Clearings,
                            when Mountain => Mountain_Clearings)
      );
   end New_Map;

   function Clearings_Set (Self : in out Map) return Boolean is
      (Self.Clears_Set);

   function Set_Clearing_Suits (
      Self  : in out Map;
      Suits :        Clearing_Suit_By_Priority
   ) return Boolean is
      F, M, R : UInt8 := 0;
   begin
      for C of Suits loop
         case C is
            when Fox    => F := @ + 1;
            when Mouse  => M := @ + 1;
            when Rabbit => R := @ + 1;
         end case;
      end loop;

      if (F /= 4) or else (M /= 4) or else (R /= 4) then
         --  TODO: Send message to controlling client
         Put_Msg (Warning, "MAPS . SET_CLEARING_SUITS: "
                & "Suits are not evenly distributed.");
         return False;
      end if;

      for Clearing in Self.Clearings'Range loop
         Self.Clearings (Clearing).Suit := Suits (Clearing);
      end loop;
      return True;
   end Set_Clearing_Suits;

   procedure Place_Warriors (Self         : in out Map;
                             S            :        Seat;
                             Clearing     :        Priority;
                             Num_Warriors :        UInt8) is
   begin
      Self.Clearings (Clearing).Warriors (S) :=
         Self.Clearings (Clearing).Warriors (S) + Num_Warriors;
   end Place_Warriors;

   procedure Remove_Warriors (Self         : in out Map;
                              S            :        Seat;
                              Clearing     :        Priority;
                              Num_Warriors :        UInt8) is
      Curr_Warriors : constant UInt8 := Self.Clearings (Clearing).Warriors (S);
   begin
      Self.Clearings (Clearing).Warriors (S) :=
         (if   Curr_Warriors > Num_Warriors
          then Curr_Warriors - Num_Warriors
          else 0);
   end Remove_Warriors;

   function Count_For_Rule (Self         : Map;
                            Clearing     : Priority;
                            Count_Tokens : Boolean_By_Seat)
      return Total_By_Seat is
      C      : constant Maps.Clearing := Self.Clearings (Clearing);
      Totals :          Total_By_Seat := [others => 0];
   begin
      for S in Seat'Range loop
         Totals (S) := C.Warriors (S)
                     + C.Buildings (S)
                     + (if Count_Tokens (S) then C.Tokens (S)
                                            else 0);
      end loop;
      return Totals;
   end Count_For_Rule;

   function Get_Current_Map return Map_Access is
      (Current_Map);

end Maps;

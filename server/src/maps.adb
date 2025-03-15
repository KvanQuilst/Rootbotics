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
with Logs; use Logs;

package body Maps is

   -----------------
   -- Map Methods --
   -----------------

   -- Constructors --
   function New_Map (M_Type : Map_Type) return Map is
      (M_Type      => M_Type,
       Item_Supply => <>,
       Clearings   => (case M_Type is
                          when Fall     => Fall_Clearings,
                          when Winter   => Winter_Clearings,
                          when Lake     => Lake_Clearings,
                          when Mountain => Mountain_Clearings));

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

end Maps;

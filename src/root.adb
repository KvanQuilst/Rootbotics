-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                               ROOT (Body)                                 --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains general structures and information for The Rootbotics  --
-- Assistant.                                                                --
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Root is
   package Int_IO is new Integer_IO (Integer); use Int_IO;

   procedure Deploy_Warriors (Supply       : in out Integer;
                              Map_Warriors : in out Warrior_Arr;
                              Clear        :        Priority;
                              Num_Warriors :        Integer) is
   begin
      if Supply >= Num_Warriors then
         Put_Line ("Place" & Num_Warriors'Image & " warriors in clearing" &
                   Clear'Image & ".");
         Map_Warriors (Clear) := Map_Warriors (Clear) + Num_Warriors;
         Supply := Supply - Num_Warriors;
      elsif Supply > 0 then
         Put_Line ("Place" & Supply'Image & " warriors in clearing" &
                   Clear'Image & ".");
         Map_Warriors (Clear) := Map_Warriors (Clear) + Supply;
         Supply := 0;
      else
         Put_Line ("Unable to place warriors: warrior supply is depleted!");
      end if;
   end Deploy_Warriors;

   procedure Deploy_Building (Supply     : in out Integer;
                              Map_Builds : in out Building_Arr;
                              Clear      :        Priority;
                              Build_Type :        String) is
   begin
      if Supply > 0 and then Map_Builds (Clear) < 3 then
         Put_Line ("Place a " & Build_Type & " in clearing" &
                   Clear'Image & ".");
         Map_Builds (Clear) := Map_Builds (Clear) + 1;
         Supply := Supply - 1;
      else
         Put_Line ("Unable to place building: building supply is depleted!");
      end if;
   end Deploy_Building;

   function Get_List_Internal (PL : out Priority_List) return Boolean is
      Line : Unbounded_String;
      Last : Integer := 0;
      Count : Integer := 1;
   begin
      for I in PL'Range loop
         PL (I) := 0;
      end loop;

      Line := To_Unbounded_String (Get_Line);

      while Last /= Length (Line) loop
         Get (Slice (Line, Last + 1, Length (Line)), PL (Count), Last);
         Count := Count + 1;
      end loop;
      return True;
   exception
      when others =>
         return False;
   end Get_List_Internal;

   -- Get space serparated list of priority (clearings) --
   -- Checks for errors                     --
   function Get_List (Values : String) return Priority_List is
      PL : Priority_List;
   begin
      Put_Line ("Provide a space separated list of " & Values & ":");
      while not Get_List_Internal (PL) loop
         Put_Line ("Invalid input!");
         Put_Line ("Provide a space separated list of " & Values & ":");
      end loop;
      return PL;
   end Get_List;

end Root;

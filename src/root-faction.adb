-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Body)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the common faction-related       --
-- subroutines used throughout The Rootbotics Assistnat.                     --
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
package body Root.Faction is

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

end Root.Faction;

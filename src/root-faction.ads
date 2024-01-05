-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Spec)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the common faction-related        --
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
package Root.Faction is

   type Warrior_Arr  is array (Priority'Range) of Natural;
   type Building_Arr is array (Priority'Range) of Integer range 0 .. 3;
   type Rule_Arr     is array (Priority'Range) of Boolean;

   ---------------------------------
   -- Faction Resource Management --
   ---------------------------------
   procedure Deploy_Warriors (Supply       : in out Integer;
                              Map_Warriors : in out Warrior_Arr;
                              Clear        :        Priority;
                              Num_Warriors :        Integer);
   procedure Deploy_Building (Supply     : in out Integer;
                              Map_Builds : in out Building_Arr;
                              Clear      :        Priority;
                              Build_Type :        String);

end Root.Faction;

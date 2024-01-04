-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                               ROOT (Spec)                                 --
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
package Root is

   type Difficulty is (Easy, Default, Challenging, Nightmare);

   type Suit  is (Fox, Mouse, Rabbit, Bird);
   type Phase is (Birdsong, Daylight, Evening, None);
   subtype Clearing_Suit is Suit range Fox .. Rabbit;
   subtype Priority is Integer range 1 .. 12;

   type Meeple_Arr is array (Priority'Range) of Natural;
   type Warrior_Arr  is array (Priority'Range) of Natural;
   type Building_Arr is array (Priority'Range) of Integer range 0 .. 3;
   type Rule_Arr     is array (Priority'Range) of Boolean;

   type Priority_List is array (Priority'Range)
     of Integer range 0 .. 12;

   type Help_Procedure is access procedure;
   Help : Help_Procedure := null;

   -- Faction Resource Management --
   procedure Deploy_Warriors (Supply       : in out Integer;
                              Map_Warriors : in out Warrior_Arr;
                              Clear        :        Priority;
                              Num_Warriors :        Integer);
   procedure Deploy_Building (Supply     : in out Integer;
                              Map_Builds : in out Building_Arr;
                              Clear      :        Priority;
                              Build_Type :        String);

   function  Get_List (Values : String) return Priority_List;

end Root;

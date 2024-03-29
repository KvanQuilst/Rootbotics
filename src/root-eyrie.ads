-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                           ROOT . EYRIE (Spec)                             --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Electric Eyrie faction from  --
-- Root: The Clockwork Expansion for use in The Rootbotics Assistant.        --
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
with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package Root.Eyrie is

   Name : constant String := String_Style ("Electric Eyrie", B_Blue);

   procedure Setup;
   procedure Take_Turn (Order : Suit; M : Map_Old);

private

   MEEPLE_MAX : constant := 20;
   ROOST_MAX  : constant := 7;

   Meeple_Supply : Integer range 0 .. MEEPLE_MAX := MEEPLE_MAX - 6;
   Roost_Supply  : Integer range 0 .. ROOST_MAX  := ROOST_MAX - 1;

   Meeples : array (Priority'Range) of Natural;
   Roosts  : array (Priority'Range) of Boolean;
   Decrees : array (Suit) of Natural := (Bird => 2, others => 0);
   Roost_Points  : Integer range 0 .. 6 := 0;

end Root.Eyrie;

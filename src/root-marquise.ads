-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . MARQUISE (Spec)                           --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Mechanical Marquise 2.0      --
-- factions from Root: The Clockwork Expansion for use in The Rootbotics     --
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
with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package Root.Marquise is

   Name : constant String := String_Style ("Mechanical Marquise 2.0", Yellow);

   procedure Setup     (M : Map);
   procedure Take_Turn (Order : Suit; M : Map);

private

   MEEPLE_MAX    : constant := 25;
   BUILDINGS_MAX : constant := 6;

   Meeple_Supply    : Integer range 0 .. MEEPLE_MAX := MEEPLE_MAX - 12;

   --  Meeples   : array (Priority'Range) of Integer range 0 .. MEEPLE_MAX;
   Meeples : Meeple_Arr;
   Rule      : array (Priority'Range) of Boolean;

   type Building is (Sawmill, Workshop, Recruiter);

   Buildings : array (Building'Range, Priority'Range) of
     Integer range 0 .. 3;

   Building_Supply : array (Building'Range) of
      Integer range 0 .. BUILDINGS_MAX;

end Root.Marquise;

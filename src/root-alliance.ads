-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . ALLIANCE (Spec)                           --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Automated Alliance faction   --
-- from Root: The Clockwork Expansion for use in The Rootbotics Assistant.   --
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

package Root.Alliance is

   Name : constant String := String_Style ("Automated Alliance", Green);

   function  Setup (Diff : Difficulty) return Boolean;
   procedure Take_Turn (Order : Suit; M : Map);

private

   MEEPLE_MAX : constant := 10;
   SYMPATHY_MAX : constant := 10;

   Meeple_Supply   : Integer range 0 .. MEEPLE_MAX  := MEEPLE_MAX;
   Sympathy_Supply : Integer range 0 .. SYMPATHY_MAX := SYMPATHY_MAX;

   Meeples    : array (Priority'Range) of Natural;
   Sympathies : array (Priority'Range) of Boolean;

end Root.Alliance;

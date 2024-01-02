-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . ALLIANCE (Body)                           --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the Automated Alliance faction   --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Root.Alliance is

   procedure Put_Logo is
      Length : constant := 16;
   begin
      Set_Style (Green);
      Put_Line_Centered ("      _       _      ");
      Put_Line_Centered ("     / \     / \     ");
      Put_Line_Centered ("    |/ \|   |/ \|    ");
      Put_Line_Centered ("    || ||   || ||    ");
      Put_Line_Centered ("   _|| ||___|| ||_   ");
      Put_Line_Centered ("  /  __       __   \ ");
      Put_Line_Centered (" / /   /\   /   /\  \");
      Put_Line_Centered ("| |   <  | |   <  | |");
      Put_Line_Centered ("|  \ __\/   \ __\/  |");
      Put_Line_Centered (" \  . .   |    . .  /");
      Reset_Style;
      Put (To_String ((WIDTH - Length) / 2 * "-"));
      Set_Style (Green);
      Put               ("\________________/");
      Reset_Style;
      Put_Line (To_String ((WIDTH - Length) / 2 * "-"));
   end Put_Logo;

   -------------------
   -- Faction Setup --
   -------------------

   function Setup (Diff : Difficulty) return Boolean is
   begin
      return True;
   end Setup;

   ---------------
   -- Take Turn --
   ---------------

   procedure Take_Turn (Order : Suit; M : Map) is
   begin

      -- Alliance State --
      Put_Logo;
      New_Line;
      Set_Style (Green);
      Put_Line_Centered ("Automated Alliance");
      Reset_Style;
      -- F  M  R --
      -- X  X  X --
      -- Sympathetic Clearings: X --
      New_Line;
      Separator;

      -- Birdsong --
      Put_Birdsong;

      Continue;

      -- Daylight --
      Put_Daylight;

      Continue;

      -- Evening --
      Put_Evening;

      Continue;

   end Take_Turn;

end Root.Alliance;

-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                           ROOT . COLOR (Spac)                             --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file specifies the coloring strategies used in the Rootbatics        --
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
with IO_Utils.Ansi; use IO_Utils.Ansi;

package Root.Color is

   type Color_Setting is (Base, EightBit, Truecolor);
   C_Setting : Color_Setting := Base;

   Default_Color : constant Color_Elem := (Color_T, Default);

   --------------
   -- Factions --
   --------------
   function Alliance_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      Green),
         when EightBit  => (Color_8_T,    28),
         when Truecolor => (Color_RGB_T, (19, 162, 85)));

   function Lizard_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      B_Yellow),
         when EightBit  => (Color_8_T,    227),
         when Truecolor => (Color_RGB_T, (249, 240, 100)));

   function Duchy_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      B_Magenta),
         when EightBit  => (Color_8_T,    223),
         when Truecolor => (Color_RGB_T, (237, 195, 163)));

   -----------
   -- Suits --
   -----------
   function Fox_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      Red),
         when EightBit  => (Color_8_T,    167),
         when Truecolor => (Color_RGB_T, (221, 79, 57)));

   function Mouse_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      Yellow),
         when EightBit  => (Color_8_T,    214),
         when Truecolor => (Color_RGB_T, (251, 157, 105)));

   function Rabbit_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      B_Yellow),
         when EightBit  => (Color_8_T,    222),
         when Truecolor => (Color_RGB_T, (255, 232, 100)));

   function Bird_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,      B_Blue),
         when EightBit  => (Color_8_T,    30),
         when Truecolor => (Color_RGB_T, (105, 190, 193)));

   ------------
   -- Phases --
   ------------
   function Birdsong_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,       Yellow),
         when EightBit  => (Color_8_T,     255),
         when Truecolor => (Color_RGB_T, (255, 255, 255)));

   function Daylight_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,       B_Cyan),
         when EightBit  => (Color_8_T,     255),
         when Truecolor => (Color_RGB_T, (255, 255, 255)));

   function Evening_Color return Color_Elem is
      (case C_Setting is
         when Base      => (Color_T,       B_Black),
         when EightBit  => (Color_8_T,     255),
         when Truecolor => (Color_RGB_T, (255, 255, 255)));

end Root.Color;

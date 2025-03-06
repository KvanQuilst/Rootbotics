-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                               ROOT (Spec)                                 --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
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
with Types; use Types;

package Root is

   ----------------------
   -- Enumerated Types --
   ----------------------

   type Map_Type is (
      Fall,
      Winter,
      Lake,
      Mountain
   ) with Size => 8;

   for Map_Type use (
      Fall     => 0,
      Winter   => 1,
      Lake     => 2,
      Mountain => 3
   );

   type Suit is (
      Fox,
      Rabbit,
      Mouse,
      Bird
   ) with Size => 8;

   for Suit use (
      Fox    => 0,
      Rabbit => 1,
      Mouse  => 2,
      Bird   => 3
   );

   type Item is (
      Boot,
      Bag,
      Crossbow,
      Hammer,
      Sword,
      Tea,
      Coins
   ) with Size => 8;

   for Item use (
      Boot     => 0,
      Bag      => 1,
      Crossbow => 2,
      Hammer   => 3,
      Sword    => 4,
      Tea      => 5,
      Coins    => 6
   );

   type Faction_Type is (
      Marquise,
      Eyrie,
      Alliance,
      Vagabot,
      Lizards,
      Riverfolk,
      Duchy,
      Corvids
   ) with Size => 8;

   for Faction_Type use (
      Marquise  => 0,
      Eyrie     => 1,
      Alliance  => 2,
      Vagabot   => 3,
      Lizards   => 4,
      Riverfolk => 5,
      Duchy     => 6,
      Corvids   => 7
   );

   type Difficulty is (
      Easy,
      Normal,
      Challenging,
      Nightmare
   ) with Size => 8;

   for Difficulty use (
      Easy        => 0,
      Normal      => 1,
      Challenging => 2,
      Nightmare   => 3
   );

   type Phase is (Birdsong, Daylight, Evening, None);

   subtype Priority is UInt8 range 1 .. 12;

   type Priority_Arr  is array (Priority range <>) of Priority;
   type Priority_List is array (Priority'Range) of UInt8 range 0 .. 12;

   type Meeple_Arr    is array (Priority'Range) of UInt8;

   subtype Seat is UInt8 range 1 .. 6;

end Root;

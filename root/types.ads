-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               TYPES (Spec)                                --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for common mundane types used        --
-- throughout the project.                                                   --
--                                                                           --
-- The Root Faction Assistant is free software: you can redistribute it      --
-- and/or modify it under the terms of the GNU General Public License as     --
-- published by the Free Software Foundation, either version 3 of the        --
-- License, or (at your option) any later version.                           --
--                                                                           --
-- The Root Faction Assistant is distributed in the hope that it will be     --
-- useful, but WITHOUT ANY WARRANTY; wihtout even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General  --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Assistant. If not, see                                --
-- <https://www.gnu.org/licenses/>.                                          --
-------------------------------------------------------------------------------
package Types is

   type UInt2 is mod 2 ** 2
      with Size => 2;
   type UInt4 is mod 2 ** 4
      with Size => 4;
   type UInt8 is mod 2 ** 8
      with Size => 8;
   type UInt16 is mod 2 ** 16
      with Size => 16;

   --
   --  Intended Usage:
   --  V : UInt8 := 16#00#;
   --  B : Bitfield (0 .. V'Size - 1)
   --     with Address => V'Address, Import, Volatile;
   --
   type Bitfield is array (Natural range <>) of Boolean
      with Pack;

end Types;

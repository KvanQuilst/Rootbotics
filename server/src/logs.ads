-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               LOGS (Spec)                                 --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the common faction-related        --
-- subroutines used throughout The Rootbotics Assistant.                     --
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
with Types; use Types;

package Logs is

   type Log_Levels is (
      Error,
      Warning,
      Info,
      Verbose,
      Debug
   );

   for Log_Levels use (
      Error   => 0,
      Warning => 1,
      Info    => 2,
      Verbose => 3,
      Debug   => 4
   );

   type Log_Level_Arr is array (Natural range <>) of Log_Levels;

   procedure Set_Log_Level (Levels : Log_Level_Arr);
   procedure Put_Msg (Level : Log_Levels;
                      Msg   : String);

private

   L          : UInt4 := 2#0011#;
   Log_Level  : Bitfield (0 .. L'Size - 1)
      with Address => L'Address, Import, Volatile;

end Logs;

-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                               LOGS (Body)                                 --
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
with Ada.Text_IO; use Ada.Text_IO;

package body Logs is

   procedure Set_Log_Level (Levels : Log_Level_Arr) is
   begin
      L := 0;
      for L of Levels loop
         Log_Level (Log_Levels'Pos (L)) := True;
      end loop;
   end Set_Log_Level;

   procedure Put_Msg (Level : Log_Levels;
                      Msg   : String) is
      Tag : constant String := (case Level is
                                 when Error   => "> Error: ",
                                 when Warning => "> Warning: ",
                                 when Info    => "> Info: ",
                                 when Verbose => "> ",
                                 when Debug   => "> Debug: ");
   begin
      if Log_Level (Log_Levels'Pos (Level)) then
         Put_Line (Tag & Msg);
      end if;
   end Put_Msg;

end Logs;

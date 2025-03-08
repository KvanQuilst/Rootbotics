-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                         FACTION . CW_DUCHY (Spec)                         --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the Drillbit Duchy faction.       --
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
with Root.Messages; use Root.Messages;

package Faction.CW_Duchy is

   type Drillbit_Duchy is new Faction and Serializable with private;

   overriding
   function  Msg_Length  (Self : Drillbit_Duchy) return UInt8   is (0);
   overriding
   function  Serialize   (Self : Drillbit_Duchy) return Payload is (0, 0);
   overriding
   procedure Deserialize (Self : Drillbit_Duchy)                is null;

private

   type Drillbit_Duchy is new Faction (Duchy) and Serializable with
      record
         null;
      end record;

end Faction.CW_Duchy;

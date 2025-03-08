-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                       FACTION . CW_ALLIANCE (Spec)                        --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the Automated Alliance faction.   --
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

package Faction.CW_Alliance is

   type Automated_Alliance is new Faction and Serializable with private;

   overriding
   function  Msg_Length  (Self : Automated_Alliance) return UInt8   is (0);
   overriding
   function  Serialize   (Self : Automated_Alliance) return Payload is (0, 0);
   overriding
   procedure Deserialize (Self : Automated_Alliance)                is null;

private

   subtype Warrior          is UInt8 range 0 .. 10;
   type    Warrior_By_Clear is array (Priority) of Warrior;

   subtype Sympathy          is UInt8 range 0 .. 10;

   type Base_Supply_By_Suit is array (Clearing_Suit) of Boolean;
   type Base_Clears_By_Suit is array (Clearing_Suit) of Priority;

   type Automated_Alliance is new Faction (Alliance) and Serializable with
      record
         Warrior_Supply     : Warrior             := Warrior'Last;
         Map_Warriors       : Warrior_By_Clear    := (others => 0);
         Sympathy_Supply    : Sympathy            := Sympathy'Last;
         Sympathetic_Clears : Boolean_By_Priority := (others => False);
         Base_Supply        : Base_Supply_By_Suit := (others => True);
         Base_Clears        : Base_Clears_By_Suit := (others => 1);
      end record;

end Faction.CW_Alliance;

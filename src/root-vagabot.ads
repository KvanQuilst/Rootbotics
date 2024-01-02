-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . VAGABOT (Spec)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Vagabot faction from Root:   --
-- The Clockwork Expansion for use in The Rootbotics Assistant.              --
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

package Root.Vagabot is

   Name : constant String := String_Style ("Vagabot", White);

   procedure Setup;
   procedure Take_Turn (Order : Suit; M : Map);

private

   MAX_ITEMS : constant := 20;

   Num_Items : Positive := 4;

   type V_Character is (Thief, Tinker, Ranger, Vagrant, Scoundrel, Arbiter);

   type Item_State  is (Exhausted, Unexhausted, Empty);
   type Item_Arr    is array (Integer range 1 .. MAX_ITEMS) of Item_State;
   subtype Item_Idx is Integer range 0 .. MAX_ITEMS;

   Special : access procedure;

   -- Order: [Unexhausted items, Exhausted items] --
   Undamaged : Item_Arr := (others => Empty);
   Undamaged_Idx : Item_Idx;

   -- Order: None --
   Damaged   : Item_Arr := (others => Empty);
   Damaged_Idx : Item_Idx;

end Root.Vagabot;

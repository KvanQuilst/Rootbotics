-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . LIZARDS (Spec)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Logical Lizards faction from --
-- Root: The Clockwork Expansion 2 for use in The Rootbotics Assistant.      --
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

with Root.Faction; use Root.Faction;
with Root.IO; use Root.IO;

package Root.Lizards is

   Faction_Color : constant Color := B_Yellow;
   Name : constant String := String_Style ("Logical Lizards", Faction_Color);

   procedure Setup;
   procedure Take_Turn;

private

   WARRIOR_MAX  : constant Integer := 25;
   GARDENS_MAX  : constant Integer := 5;

   type Conspiracy is (Convert, Crusade, Sanctify);
   type Conspiracy_Count is mod 5;
   type Action is (Outcasts, Convert, Crusade, Sanctify, Rituals, None);
   subtype Garden is Building_Suit;

   procedure Put_Phase with Inline;
   procedure Prompt    with Inline;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Warrior_Supply  : Integer range 0 .. WARRIOR_MAX := WARRIOR_MAX;
   Map_Warriors    : Warrior_Arr;
   Rule            : Rule_Arr;
   Acolytes        : Integer range 0 .. WARRIOR_MAX := 0;
   Next_Conspiracy : Conspiracy_Count := 0;
   Conspiracies    : constant array (Conspiracy_Count'Range) of Conspiracy :=
                        (Convert, Crusade, Convert, Crusade, Sanctify);
   Gardens         : Building_Arr;
   Garden_Supply   : Suit_Build_Supply := (others => GARDENS_MAX);

   Curr_Order  : Suit;
   Curr_Phase  : Phase;
   Curr_Action : Action;

   Logo_Width : constant := 24;
   --  Logo : constant array (Integer range <>) of Unbounded_String :=
   Logo : Logo_Arr :=
      (Unbounded (String_Style ("                        ", Faction_Color)),
       Unbounded (String_Style ("      ___               ", Faction_Color)),
       Unbounded (String_Style ("    / ___ \___          ", Faction_Color)),
       Unbounded (String_Style (" </  /\  \     \___     ", Faction_Color)),
       Unbounded (String_Style ("<|   \/__/          \^\ ", Faction_Color)),
       Unbounded (String_Style (" | .   ________________\", Faction_Color)),
       Unbounded (String_Style ("<|    ___    | |      / ", Faction_Color)),
       Unbounded (String_Style (" <|  /   \    ^      /  ", Faction_Color)),
       Unbounded (String_Style ("   |_\___/_________/    ", Faction_Color)));

   -- Phases --
   procedure Birdsong;
   procedure Daylight;
   procedure Evening;

   -- Actions --
   procedure Outcasts;
   procedure Do_Conspiracies;
   procedure Ritual (S : Suit);

end Root.Lizards;

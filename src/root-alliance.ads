-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                         ROOT . ALLIANCE (Spec)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the Automated Alliance faction   --
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
with IO_Utils.Ansi; use IO_Utils.Ansi;
with IO_Utils.Strings; use IO_Utils.Strings;
with IO_Utils.User_IO; use IO_Utils.User_IO;

with Root.Color; use Root.Color;
with Root.Faction; use Root.Faction;

package Root.Alliance is

   Name_Plain : constant String := "Automated Alliance";

   function Faction_Color return Color_Elem renames Alliance_Color;
   function Name          return String is
      (Set_Fg (Name_Plain, Faction_Color));

   procedure Setup;
   procedure Take_Turn;

private

   WARRIOR_MAX  : constant Integer := 10;
   SYMPATHY_MAX : constant Integer := 10;
   FORTS_MAX    : constant Integer := 1;

   type    Action is (Revolt, Spread_Sympathy, Organize, Recruit,
                      Reveal, Craft, None);
   subtype Fort is Building_Suit;

   procedure Put_Phase with Inline;
   procedure Prompt    with Inline;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Warrior_Supply  : Integer range 0 .. WARRIOR_MAX  := WARRIOR_MAX;
   Sympathy_Supply : Integer range 0 .. SYMPATHY_MAX := SYMPATHY_MAX;
   Officers        : Integer range 0 .. WARRIOR_MAX  := 0;
   Fort_Supply     : Suit_Build_Supply := (others => FORTS_MAX);

   Map_Warriors    : Warrior_Arr  := (others => 0);
   Forts           : Building_Arr := (others => 0);
   Map_Sympathy    : Token_Arr    := (others => False);
   Rule            : Rule_Arr     := (others => False);

   Curr_Order  : Suit;
   Curr_Phase  : Phase  := None;
   Curr_Action : Action := None;

   Logo_Width : constant := 21;
   function Logo return Logo_Arr is
      (Unbounded (Set_Fg ("      _       _      ", Faction_Color)),
       Unbounded (Set_Fg ("     / \     / \     ", Faction_Color)),
       Unbounded (Set_Fg ("    |/ \|   |/ \|    ", Faction_Color)),
       Unbounded (Set_Fg ("    || ||   || ||    ", Faction_Color)),
       Unbounded (Set_Fg ("   _|| ||___|| ||_   ", Faction_Color)),
       Unbounded (Set_Fg ("  /  __       __  \  ", Faction_Color)),
       Unbounded (Set_Fg (" / /   /\   /   /\ \ ", Faction_Color)),
       Unbounded (Set_Fg ("| |   <  | |   <  | |", Faction_Color)),
       Unbounded (Set_Fg ("|  \ __\/   \ __\/  |", Faction_Color)),
       Unbounded (Set_Fg (" \  . .   |   . .  / ", Faction_Color)),
       Unbounded (Set_Fg ("  \_______________/  ", Faction_Color)));

   -- Phases --
   procedure Birdsong;
   procedure Daylight;
   procedure Evening;

   -- Actions --
   procedure Reveal;
   function  Revolt (Clears : Int_Arr) return Boolean;
   procedure Spread_Sympathy;
   procedure Organize;
   procedure Recruit;

end Root.Alliance;

-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                           ROOT . DUCHY (Spec)                             --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
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

with Root.Color; use Root.Color;
with Root.Faction; use Root.Faction;

package Root.Duchy is

   Name_Plain : constant String := "Drillbit Duchy";

   function Faction_Color return Color_Elem renames Duchy_Color;
   function Name          return String is
      (Set_Fg (Name_Plain, Faction_Color));

   procedure Setup;
   procedure Take_Turn;

private

   WARRIOR_MAX : constant Integer := 20;
   CROWN_MAX   : constant Integer := 9;
   TUNNEL_MAX  : constant Integer := 3;
   BUILD_MAX   : constant Integer := 3;

   type Trait    is (Foundations, Invaders, Investors, Overwhelm);
   type Action   is (Reveal, Craft, Recruit,
                     Dig, Battle, Build, Ministers,
                     Rally, Score, Sway,
                     None);
   type Minister is (Captain, Marshal, Foremole, Brigadier, Banker,
                     Mayor, Earl_of_Stone, Baron_of_Dirt, Duchess_of_Mud);
   type Building is (Citadel, Market);

   procedure Put_Phase with Inline;
   procedure Prompt    with Inline;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   Warrior_Supply : Integer range 0 .. WARRIOR_MAX := WARRIOR_MAX;
   Crown_Supply   : Integer range 0 .. CROWN_MAX   := CROWN_MAX;
   Citadel_Supply : Integer range 0 .. BUILD_MAX   := BUILD_MAX;
   Market_Supply  : Integer range 0 .. BUILD_MAX   := BUILD_MAX;
   Tunnel_Supply  : Integer range 0 .. TUNNEL_MAX  := TUNNEL_MAX;
   Burrow         : Integer range 0 .. WARRIOR_MAX := 0;

   Map_Warriors   : Warrior_Arr   := (others => 0);
   Map_Buildings  : Building_Arr  := (others => 0);
   Map_Citadels   : Building_Arr  := (others => 0);
   Map_Markets    : Building_Arr  := (others => 0);
   Map_Tunnels    : Token_Arr     := (others => False);
   Rule           : Rule_Arr      := (others => False);

   Traits : array (Trait range Trait'First .. Trait'Last) of Boolean :=
      (others => False);

   Diff        : Difficulty;
   Curr_Order  : Suit;
   Curr_Phase  : Phase := None;
   Curr_Action : Action := None;

   type Minister_Arr is array (Integer range 1 .. 3) of Minister;
   Swayed_Ministers : array (Minister'Range) of Boolean := (others => False);
   Suit_Ministers   : array (Clearing_Suit'Range) of Minister_Arr :=
         (Fox    => (Captain, Brigadier, Earl_of_Stone),
          Rabbit => (Marshal, Banker, Baron_of_Dirt),
          Mouse  => (Foremole, Mayor, Duchess_of_Mud));
   Minister_Str : array (Minister'Range) of Unbounded_String :=
      (Unbounded ("Captain"),
       Unbounded ("Marshal"),
       Unbounded ("Foremole"),
       Unbounded ("Brigadier"),
       Unbounded ("Banker"),
       Unbounded ("Mayor"),
       Unbounded ("Earl of Stone"),
       Unbounded ("Baron of Dirt"),
       Unbounded ("Duchess of Mud"));

   Logo_Width : constant := 24;
   function Logo return Logo_Arr is
      (Unbounded (Set_Fg ("                        ", Faction_Color)),
       Unbounded (Set_Fg ("   ____________         ", Faction_Color)),
       Unbounded (Set_Fg ("  /            \   \\/\ ", Faction_Color)),
       Unbounded (Set_Fg (" /     __        \_/ //|", Faction_Color)),
       Unbounded (Set_Fg ("|     /               /=", Faction_Color)),
       Unbounded (Set_Fg ("|           _________|  ", Faction_Color)),
       Unbounded (Set_Fg ("|           V       |   ", Faction_Color)),
       Unbounded (Set_Fg ("|                   |   ", Faction_Color)),
       Unbounded (Set_Fg (" \__________________\   ", Faction_Color)));

   -- Phase --
   procedure Birdsong;
   procedure Daylight;
   procedure Evening;

   -- Actions --
   procedure Cost_of_Errors (S : Suit);

   procedure Reveal;
   procedure Recruit;

   procedure Dig (S : Suit);
   procedure Battle (S : Suit);
   procedure Build;
   procedure Ministers;

   procedure Rally;
   procedure Score;
   procedure Sway_Minister (S : Suit);

end Root.Duchy;

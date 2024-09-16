-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . FACTION (Spec)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the common faction-related        --
-- subroutines used throughout The Rootbotics Assistnat.                     --
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

package Root.Faction is

   type Warrior_Arr  is array (Priority'Range) of Natural;
   type Building_Arr is array (Priority'Range) of Integer range 0 .. 3;
   type Token_Arr    is array (Priority'Range) of Boolean;
   type Rule_Arr     is array (Priority'Range) of Boolean;

   subtype Building_Suit is Suit range Fox .. Mouse;
   type Suit_Build_Supply is array (Building_Suit'Range) of Integer;

   type Difficulty is (Easy, Normal, Challenging, Nightmare);

   ----------------
   -- Faction IO --
   ----------------
   type Logo_Arr is array (Integer range <>) of Unbounded_String;
   procedure Put_Logo (Name       : String;
                       Logo       : Logo_Arr;
                       Logo_Width : Positive);

   ---------------------------------
   -- Faction Resource Management --
   ---------------------------------
   function Check_Difficulty (Name : String) return Difficulty;

   function Check_Warriors  (Prompt       : access procedure;
                             Supply       : in out   Natural;
                             Map_Warriors : in out   Warrior_Arr;
                             Max_Warriors :          Integer) return Natural;
   function Check_Buildings (Prompt : access procedure;
                             Supply : in out Suit_Build_Supply;
                             Builds : in out Building_Arr;
                             Max_Builds : Integer) return Natural;
   procedure Check_Tokens   (Prompt : access procedure;
                             Supply : in out Natural;
                             Tokens : in out Token_Arr);
   procedure Check_Rule     (Prompt : access procedure;
                             Rule   : in out Rule_Arr);

   procedure Move_Warriors   (Map_Warriors : in out Warrior_Arr;
                              Rule         : in out Rule_Arr;
                              To, From     :        Priority;
                              Num_Warriors :        Natural;
                              Name         :        String;
                              Prompt       : access procedure);
   procedure Deploy_Warriors (Supply       : in out Natural;
                              Map_Warriors : in out Warrior_Arr;
                              Clear        :        Priority;
                              Num_Warriors :        Positive);
   function Deploy_Building  (Supply       : in out Natural;
                              Map_Builds   : in out Building_Arr;
                              Clear        :        Priority;
                              Build_Type   :        String) return Boolean;

end Root.Faction;

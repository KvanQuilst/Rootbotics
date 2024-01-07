-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . ALLIANCE (Body)                           --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the Automated Alliance faction   --
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
package body Root.Alliance is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
   begin
      Root.Faction.Put_Logo (Name, Logo, Logo_Width);
   end Put_Logo;

   procedure Put_State is null;

   procedure Prompt (Time : Phase := None) is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Forts, Rule, Curr_Order, Time);
   end Prompt;

   --------------------
   -- Alliance Setup --
   --------------------

   procedure Setup is null;

   ---------------
   -- Take Turn --
   ---------------
   procedure Birdsong is null;
   procedure Daylight is null;
   procedure Evening  is null;

   procedure Take_Turn (Order : Suit) is
   begin
      Curr_Order := Order;

      ----------------------
      -- Confirm Warriors --
      ----------------------
      declare
         Lost : constant Natural :=
            Check_Warriors (Prompt'Access, Warrior_Supply,
                            Map_Warriors, WARRIOR_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      -----------------------
      -- Confirm Buildings --
      -----------------------
      declare
         Lost : constant Natural :=
            Check_Buildings (Prompt'Access, Fort_Supply, Forts, FORTS_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      ------------------
      -- Confirm Rule --
      ------------------
      Check_Rule (Prompt'Access, Rule);

      --------------
      -- Birdsong --
      --------------
      Prompt (Birdsong);
      Birdsong;
      Continue;

      --------------
      -- Daylight --
      --------------
      Prompt (Daylight);
      Daylight;
      Continue;

      -------------
      -- Evening --
      -------------
      Prompt (Evening);
      Evening;
      Continue;

   end Take_Turn;

end Root.Alliance;

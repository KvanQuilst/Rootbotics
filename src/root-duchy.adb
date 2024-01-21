-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                           ROOT . DUCHY (Spec)                             --
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
with Ada.Text_IO; use Ada.Text_IO;

with Root.Maps; use Root.Maps;

package body Root.Duchy is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
   begin
      Root.Faction.Put_Logo (Name, Logo, Logo_Width);
   end Put_Logo;

   procedure Put_State is null;

   procedure Put_Phase is
   begin
      Root.IO.Put_Phase (Curr_Phase,
                        (case Curr_Action is
                           when Order   => "Set Order",
                           when Craft   => "Craft",
                           when Recruit => "Recruit",
                           when Dig     => "Dig",
                           when None    => ""));
   end Put_Phase;

   procedure Prompt is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Map_Buildings, Rule, Curr_Order, Put_Phase'Access,
                  Map_Tunnels);
   end Prompt;

   -----------------
   -- Duchy Setup --
   -----------------
   procedure Setup is
      C : Priority;
   begin
      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      New_Line;

      Put_Line ("Which clearing will the " & Name & " start in?");
      C := Get_Integer (1, 12);

      -- Starting Clearing --
      Map_Warriors (C) := 2;
      Warrior_Supply   := Warrior_Supply - 2;
      Map_Tunnels (C)  := True;
      Tunnel_Supply    := Tunnel_Supply - 1;

      for Neighbor of Clearings (C).Neighbors loop
         exit when Neighbor = 0;
         Map_Warriors (Neighbor) := 2;
         Warrior_Supply := Warrior_Supply - 2;
      end loop;

      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      New_Line;
      Put_Line ("Draw two cards. What is the suit of the first card?");
      Sway_Minister (Get_Suit_Opt);

      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      New_Line;
      Put_Line ("What is the suit of the second card?");
      Sway_Minister (Get_Suit_Opt);
      Put_Line ("Discard the cards.");
   end Setup;

   --------------------------
   -- Duchy Common Actions --
   --------------------------
   procedure Deploy_Building (Build : Building; Clear : Priority) is
   begin
      if Map_Buildings (Clear) < Clearings (Clear).Buildings then
         case Build is
            when Citadel => Root.Faction.Deploy_Building
                              (Citadel_Supply, Map_Citadels, Clear,
                               String_Style ("Citadel", Faction_Color));
            when Market => Root.Faction.Deploy_Building
                              (Market_Supply, Map_Markets, Clear,
                               String_Style ("Market", Faction_Color));
         end case;
         Map_Buildings (Clear) := Map_Buildings (Clear) + 1;
      end if;
   end Deploy_Building;

   ----------------------
   -- Duchy Turn Logic --
   ----------------------
   procedure Take_Turn is
   begin
      Curr_Order  := Bird;
      Curr_Phase  := None;
      Curr_Action := None;

      -- Confirm Warriors --
      declare
         Lost : constant Natural :=
            Check_Warriors (Prompt'Access, Warrior_Supply,
                            Map_Warriors, WARRIOR_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      -- Confirm Buildings --
      --  TODO: Deal with buildings w/ Cost of Errors

      -- Confirm Tokens --
      Check_Tokens (Prompt'Access, Tunnel_Supply, Map_Tunnels);

      -- Confirm Rule --
      Check_Rule (Prompt'Access, Rule);

      Birdsong;

      Daylight;

      Evening;

   end Take_Turn;

   -----------
   -- Phase --
   -----------
   procedure Birdsong is
   begin
      Curr_Phase := Birdsong;

      Curr_Action := Order;
      Prompt;
      Put_Line ("What is the order of this turn?");
      Curr_Order := Get_Suit_Opt;
      Continue;

      Curr_Action := Craft;
      Prompt;
      Put_Line ("Craft the order card for +1 points for the " & Name & ".");
      Continue;

      Curr_Action := Recruit;
      Recruit;
   end Birdsong;

   procedure Daylight is null;

   procedure Evening is null;

   -------------
   -- Actions --
   -------------
   procedure Sway_Minister (S : Suit) is
   begin
      if S = Bird then
         for M in Minister'Range loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crown on the " &
                         To_String (Minister_Str (M)) & ".");
               exit;
            end if;
         end loop;
      else
         for M of Suit_Ministers (S) loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crown on the " &
                         To_String (Minister_Str (M)) & ".");
               exit;
            end if;
         end loop;
      end if;
      Continue;
   end Sway_Minister;

   procedure Recruit is
      Total : constant Natural := 2 + (case Citadel_Supply is
                                          when BUILD_MAX     => 0,
                                          when BUILD_MAX - 1 => 1,
                                          when BUILD_MAX - 2 => 2,
                                          when others        => 4);
   begin
      Prompt;
      if Warrior_Supply >= Total then
         Put_Line ("Place" & Total'Image & " warriors in the Burrow.");
         Burrow := Burrow + Total;
         Warrior_Supply := Warrior_Supply - Total;
      elsif Warrior_Supply > 0 then
         Put_Line ("Place" & Warrior_Supply'Image &
                   " warriors in the Burrow.");
         Burrow := Burrow + Warrior_Supply;
         Warrior_Supply := 0;
      else
         Put_Line ("Unable to recruit warriors to the Burrow: warrior supply" &
                   " is depleted!");
      end if;
      Continue;
   end Recruit;

   procedure Dig is
      Clears  : constant Int_Arr := Filter_Clearings (Curr_Order);
      Count   :          Natural := 0;
      Clear   :          Priority;
      T_Clear :          Natural := 0;
   begin
      if Burrow < 4 then
         return;
      end if;

      Curr_Action := Dig;

      -- Determine if one clearing --
      for C of Clears loop
         if not Map_Tunnels (C) and then Map_Buildings (C) = 0 then
            Clear := C;
            Count := Count + 1;
         end if;
      end loop;

      -- Are we moving a tunnel? --
      if Tunnel_Supply = 0 then
         declare
            Min : Natural := WARRIOR_MAX;
         begin
            for C in Priority'Range loop
               if Map_Tunnels (C) and then Map_Warriors (C) < Min then
                  Min := Map_Warriors (C);
                  T_Clear := C;
               end if;
            end loop;
         end;
      end if;

      -- Player input needed --
      if Count > 1 then
         Prompt;
         declare
            Opts : String_Arr (1 .. Count);
            Idx  : Positive := 1;
         begin
            for C of Clears loop
               if not Map_Tunnels (C) and then Map_Buildings (C) = 0 then
                  Opts (Idx) := Unbounded (C'Image);
                  Idx := Idx + 1;
               end if;
            end loop;

            if Curr_Order = Bird then
               Put_Line ("Which clearing has the most enemy buildings and " &
                         "tokens?");
            else
               Put_Line ("Which clearing has the most buildings slots, then " &
                         "the fewest warriors?");
            end if;
            Clear := Character'Pos (Get_Option (Opts)) -
                                                      Character'Pos ('a') + 1;
         end;
      end if;

      Prompt;
      if T_Clear = 0 then
         Put_Line ("Place a tunnel in clearing" & Clear'Image & ".");
         Map_Tunnels (Clear) := True;
      else
         Put_Line ("Move the tunnel from clearing" & T_Clear'Image &
                   " to clearing" & Clear'Image & ".");
         Map_Tunnels (Clear) := True;
         Map_Tunnels (T_Clear) := False;
      end if;
      Put_Line ("Move 4 warriors from the Burrow to clearing" &
                Clear'Image & ".");
      Map_Warriors (Clear) := Map_Warriors (Clear) + 4;
      Burrow := Burrow - 4;
   end Dig;

end Root.Duchy;

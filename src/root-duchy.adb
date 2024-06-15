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

   procedure Put_State is
      procedure Building_State (Build : Building) is
         B_Str     : constant String := (case Build is
                                          when Citadel => " ^n",
                                          when Market  => " ()");
         Used      : constant String :=
            To_String ((BUILD_MAX - Market_Supply) * B_Str);
         Remaining : constant String :=
            To_String (Market_Supply * B_Str);
      begin
         case Build is
            when Citadel =>
               Put ("         Citadels:");
            when Market =>
               Put ("          Markets:");
         end case;
         Set_Style (B_Black);
         Put (Used);
         Set_Style (Faction_Color);
         Put (Remaining);
         Reset_Style;
         New_Line;
      end Building_State;

      procedure Minister_State is
      begin
         Put_Line ("     Ministers:");
         for I in Integer range 1 .. 3 loop
            if Swayed_Ministers (Suit_Ministers (Fox) (I)) then
               Set_Style (B_Black);
            else
               Set_Style (Suit_Color (Fox));
            end if;
            Put (To_String (Minister_Str (Suit_Ministers (Fox) (I))) & " ");
            Cursor_Column_Set (16);
            if Swayed_Ministers (Suit_Ministers (Rabbit) (I)) then
               Set_Style (B_Black);
            else
               Set_Style (Suit_Color (Rabbit));
            end if;
            Put (To_String (Minister_Str (Suit_Ministers (Rabbit) (I))) & " ");
            Cursor_Column_Set (32);
            if Swayed_Ministers (Suit_Ministers (Mouse) (I)) then
               Set_Style (B_Black);
            else
               Set_Style (Suit_Color (Mouse));
            end if;
            Put (To_String (Minister_Str (Suit_Ministers (Mouse) (I))) & " ");
            New_Line;
         end loop;
         Reset_Style;
      end Minister_State;
   begin
      Put_Line ("   Warrior Supply:" & Warrior_Supply'Image);
      Put_Line ("  Burrow Warriors:" & Burrow'Image);
      Building_State (Citadel);
      Building_State (Market);
      New_Line;
      Put_Line ("     Crown Supply:" & Crown_Supply'Image);
      Minister_State;
   end Put_State;

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

      Recruit;
   end Birdsong;

   procedure Daylight is
   begin
      null;
   end Daylight;

   procedure Evening is null;

   -------------
   -- Actions --
   -------------
   procedure Recruit is
      Total : constant Natural := 2 + (case Citadel_Supply is
                                          when BUILD_MAX     => 0,
                                          when BUILD_MAX - 1 => 1,
                                          when BUILD_MAX - 2 => 2,
                                          when others        => 4);
   begin
      Curr_Action := Recruit;
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

   procedure Dig (S : Suit) is
      Clears  : constant Int_Arr := Filter_Clearings (S);
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

            if S = Bird then
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

   procedure Battle is null;

   procedure Build is null;

   procedure Ministers is
      procedure Marshal_Action is
         Min_Set      : Boolean  := False;
         Min_Clear    : Priority := Priority'Last;
         Min_Warriors : Integer  := Integer'Last;
      begin
         for C in Map_Buildings'Range loop
            if Map_Buildings (C) > 0 then
               Min_Set := True;
               if Map_Warriors (C) < Min_Warriors then
                  Min_Clear := C;
                  Min_Warriors := Map_Warriors (C);
               end if;
            end if;
         end loop;

         if Min_Set then
            Deploy_Warriors (Warrior_Supply, Map_Warriors, Min_Clear, 1);
         end if;
      end Marshal_Action;

      procedure Brigadier_Action is
      begin
         if Burrow >= 3 then
            Dig (Bird);
         end if;
      end Brigadier_Action;

      procedure Banker_Action is
      begin
         Build;
      end Banker_Action;

      procedure Mayor_Action is
         Max_Set      : Boolean  := False;
         Max_Clear    : Priority := Priority'First;
         Max_Warriors : Integer  := 0;
      begin
         for C in Map_Warriors'Range loop
            if Map_Warriors (C) > 0 then
               Max_Set := True;
               if Map_Warriors (C) > Max_Warriors then
                  Max_Clear := C;
                  Max_Warriors := Map_Warriors (C);
               end if;
            end if;
         end loop;

         if Max_Set then
            Put_Line ("Remove 1 warrior from clearing" & Max_Clear'Image);
            Put_Score (1, Name);
            Continue;
         end if;
      end Mayor_Action;

      procedure Earl_of_Stone_Action is
         Citadels : constant Integer := BUILD_MAX - Citadel_Supply;
      begin
         if Citadels > 0 then
            Put_Score (Citadels, Name);
         end if;
      end Earl_of_Stone_Action;

      procedure Baron_of_Dirt_Action is
         Markets : constant Integer := BUILD_MAX - Market_Supply;
      begin
         if Markets > 0 then
            Put_Score (Markets, Name);
         end if;
      end Baron_of_Dirt_Action;

      procedure Duchess_of_Mud_Action is
      begin
         if Tunnel_Supply = 0 then
            Put_Score (2, Name);
         end if;
      end Duchess_of_Mud_Action;
   begin
      for M in Minister'Range loop
         -- null entries mark ministers which give new abilities --
         if Swayed_Ministers (M) then
            case M is
               when Captain        => null;
               when Marshal        => Marshal_Action;
               when Foremole        => null;
               when Brigadier      => Brigadier_Action;
               when Banker         => Banker_Action;
               when Mayor          => Mayor_Action;
               when Earl_of_Stone  => Earl_of_Stone_Action;
               when Baron_of_Dirt  => Baron_of_Dirt_Action;
               when Duchess_of_Mud => Duchess_of_Mud_Action;
            end case;
         end if;
      end loop;
   end Ministers;

   procedure Rally is null;

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

end Root.Duchy;

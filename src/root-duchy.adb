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
with IO_Utils.User_IO; use IO_Utils.User_IO;

with Root.IO; use Root.IO;
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
         Set_Fg (Root.Color.Dark_Grey);
         Put (Used);
         Set_Fg (Faction_Color);
         Put (Remaining);
         Reset_All;
         New_Line;
      end Building_State;

      procedure Minister_State is
      begin
         Put_Line ("     Ministers:");
         for I in Integer range 1 .. 3 loop
            if Swayed_Ministers (Suit_Ministers (Fox) (I)) then
               Set_Fg (Root.Color.Dark_Grey);
            else
               Set_Fg (Suit_Color (Fox));
            end if;
            Put (To_String (Minister_Str (Suit_Ministers (Fox) (I))) & " ");
            Cursor_Col_Set (16);
            if Swayed_Ministers (Suit_Ministers (Rabbit) (I)) then
               Set_Fg (Root.Color.Dark_Grey);
            else
               Set_Fg (Suit_Color (Rabbit));
            end if;
            Put (To_String (Minister_Str (Suit_Ministers (Rabbit) (I))) & " ");
            Cursor_Col_Set (32);
            if Swayed_Ministers (Suit_Ministers (Mouse) (I)) then
               Set_Fg (Root.Color.Dark_Grey);
            else
               Set_Fg (Suit_Color (Mouse));
            end if;
            Put (To_String (Minister_Str (Suit_Ministers (Mouse) (I))) & " ");
            New_Line;
         end loop;
         Reset_All;
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
                           when Reveal    => "Reveal",
                           when Craft     => "Craft",
                           when Recruit   => "Recruit",
                           when Dig       => "Dig",
                           when Battle    => "Battle",
                           when Build     => "Build",
                           when Ministers => "Ministers",
                           when Rally     => "Rally",
                           when Score     => "Score",
                           when Sway      => "Sway",
                           when None      => ""));
   end Put_Phase;

   procedure Prompt is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Map_Buildings, Rule, Curr_Order, Put_Phase'Access,
                  Map_Tunnels);
   end Prompt;

   procedure Reveal is
   begin
      Curr_Action := Reveal;
      Prompt;
      Curr_Order := Root.IO.Get_Turn_Order;
   end Reveal;

   -------------------------
   -- Duchy Common Action --
   -------------------------
   function Deploy_Building (C : Priority) return Boolean is
      Ret : Boolean;
   begin
      if Warrior_Supply >= 9 then
         Ret := Deploy_Building (Citadel_Supply,
                                 Map_Citadels,
                                 C,
                                 Citadel'Image);
      else
         Ret := Deploy_Building (Market_Supply,
                                 Map_Markets,
                                 C,
                                 Market'Image);
      end if;

      Map_Buildings (C) := Map_Buildings (C) + (if Ret then 1 else 0);
      return Ret;
   end Deploy_Building;

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

      Diff := Check_Difficulty (Name);

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

      Reveal;

      Curr_Action := Craft;
      Prompt;
      Put_Line ("Craft the order card for +1 points for the " & Name & ".");
      Continue;

      Recruit;
   end Birdsong;

   procedure Daylight is
   begin
      Curr_Phase := Daylight;

      Dig (Curr_Order);

      Battle;

      Build;

      Ministers;
   end Daylight;

   procedure Evening is
   begin
      Curr_Phase := Evening;

      Rally;

      Score;

      Sway_Minister (Curr_Order);
   end Evening;

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
            Opts : Str_Arr (1 .. Count);
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

   procedure Battle is
      Lost : Integer;
   begin
      Curr_Action := Battle;

      for C of Filter_Clearings (Curr_Order) loop
         if Map_Warriors (C) > 0 then
            Put_Line ("Are there enemy pieces in clearing" & C'Image & "?");
            if Get_Yes_No then
               Put_Line ("Battle the enemy faction with the most buildings, "
                       & "then the most pieces, then the most points "
                       & "in clearing" & C'Image & ".");
               if Swayed_Ministers (Captain) and then Map_Tunnels (C)
               then
                  Put_Line ("Deal an extra hit with the Captain Minister.");
               end if;
               Put_Line ("How many warriors were lost?");
               Lost := Get_Integer (0, Map_Warriors (C));
               Map_Warriors (C) := Map_Warriors (C) - Lost;
               Warrior_Supply := Warrior_Supply + Lost;

               Put_Line ("Do the " & Name & " rule clearing" & C'Image & "?");
               Rule (C) := Get_Yes_No;
               return;
            end if;
            Continue;
         end if;
      end loop;
   end Battle;

   procedure Build is
      Clears  : Priority_Arr (Priority'Range);
      Size    : Natural := 0;
      Success : Boolean;
   begin
      Curr_Action := Build;

      -- Get Clearings with Warriors --
      for W of Map_Warriors loop
         if W > 0 then
            Size := Size + 1;
         end if;
      end loop;

      for C in Map_Warriors'Range loop
         if Rule (C) and then Map_Warriors (C) > 0
         then
            Size := Size + 1;
            Clears (Size) := C;
         end if;
      end loop;

      -- Determine which clearing to build in --
      declare
         Clear, Clear_Idx : Priority;
      begin
         loop
            Success := True;

            -- Get Max Warriors --
            declare
               Max       : Natural := 0;
            begin
               for Idx in 1 .. Size loop
                  if Map_Warriors (Clears (Idx)) > Max then
                     Max := Map_Warriors (Clears (Idx));
                     Clear := Clears (Idx);
                     Clear_Idx := Idx;
                  end if;
               end loop;
            end;

            exit when Deploy_Building (Clear);

            -- Shift Values Down --
            for I in Clear_Idx .. Size - 1 loop
               Clears (I) := Clears (I) + 1;
            end loop;
            Size := Size - 1;

            Success := False;
         end loop;
      end;

      if not Success and then
         (Citadel_Supply > 0 or else
          Market_Supply > 0)
      then
         Put_Score (1, Name);
      end if;
   end Build;

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

      procedure Banker_Action renames Build;

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
            Put_Line ("Remove 1 warrior from clearing"
                    & Max_Clear'Image & ".");
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

      procedure Baron_of_Dirt_Action renames Score;

      procedure Duchess_of_Mud_Action is
      begin
         if Tunnel_Supply = 0 then
            Put_Score (2, Name);
         end if;
      end Duchess_of_Mud_Action;
   begin
      Curr_Action := Ministers;

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

   procedure Rally is
      Min       : Natural := WARRIOR_MAX;
      Min_Clear : Priority;
      Success   : Boolean := False;
   begin
      for C in Priority'Range loop
         if Map_Buildings (C) = 0 and then
            Map_Warriors (C) <= 2
         then
            for N of Clearings (C).Neighbors loop
               if N = 0 then
                  exit;
               end if;

               if Map_Buildings (N) > 0 and then
                  Map_Warriors (N) < Min
               then
                  Min := Map_Warriors (N);
                  Min_Clear := N;
                  Success := True;
               end if;
            end loop;

            if Success then
               Move_Warriors (Map_Warriors, Rule, C,
                              Min_Clear, Map_Warriors (C), Name);
            else
               Put_Line ("Move all warriors from clearing" & C'Image
                       & " to the burrow.");
               Burrow := Burrow + Map_Warriors (C);
               Map_Warriors (C) := 0;
            end if;
            Continue;
         end if;
      end loop;
   end Rally;

   procedure Score is
      Markets : constant Integer := BUILD_MAX - Market_Supply;
   begin
      Curr_Action := Score;

      if Markets > 0 then
         Put_Score (Markets, Name);
      end if;
   end Score;

   procedure Sway_Minister (S : Suit) is
      Swayed : Boolean := False;
   begin
      Curr_Action := Sway;

      if S = Bird then
         for M in Minister'Range loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crown on the " &
                         To_String (Minister_Str (M)) & ".");
               Swayed := True;
               exit;
            end if;
         end loop;
      else
         for M of Suit_Ministers (S) loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crown on the " &
                         To_String (Minister_Str (M)) & ".");
               Swayed := True;
               exit;
            end if;
         end loop;
      end if;

      if not Swayed then
         for M in reverse Minister'Range loop
            if not Swayed_Ministers (M) then
               Swayed_Ministers (M) := True;
               Put_Line ("Place a crow on the " &
                         To_String (Minister_Str (M)) & ".");
               Swayed := True;
            end if;
         end loop;
      end if;

      if not Swayed then
         Put_Line ("No ministers to sway.");
      end if;
      Continue;
   end Sway_Minister;

end Root.Duchy;

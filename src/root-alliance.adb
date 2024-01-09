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
with Ada.Text_IO; use Ada.Text_IO;

with Root.Maps; use Root.Maps;

package body Root.Alliance is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
   begin
      Root.Faction.Put_Logo (Name, Logo, Logo_Width);
   end Put_Logo;

   procedure Put_State is
      procedure Forts_State is
         Fort : constant String := "|^|";
      begin
         Put ("             Forts:");
         for S in Clearing_Suit'Range loop
            Set_Style ((if Fort_Supply (S) > 0
                        then (case S is
                                 when Fox    => Fox_Color,
                                 when Mouse  => Mouse_Color,
                                 when Rabbit => Rabbit_Color)
                        else B_Black));
            Put (" " & Fort);
         end loop;
         Reset_Style;
         New_Line;
      end Forts_State;

      procedure Sympathy_State is
         Sympathy : constant String := "*";
      begin
         Put ("          Sympathy:");
         for I in reverse 1 .. SYMPATHY_MAX loop
            if I mod 2 = 0 then
               if Sympathy_Supply >= I then
                  Set_Style (Green);
               else
                  Set_Style (B_Black);
               end if;
               Put ("   " & Sympathy);
            end if;
         end loop;
         New_Line;
         Put ("                 ");
         for I in reverse 1 .. SYMPATHY_MAX loop
            if I mod 2 = 1 then
               if Sympathy_Supply >= I then
                  Set_Style (Green);
               else
                  Set_Style (B_Black);
               end if;
               if I /= SYMPATHY_MAX then
                  Put ("   " & Sympathy);
               end if;
            end if;
         end loop;
         Reset_Style;
         New_Line;
      end Sympathy_State;
   begin
      Put_Line ("    Warrior Supply:" & Warrior_Supply'Image);
      Put_Line ("          Officers:" & Officers'Image);
      Forts_State;
      Sympathy_State;
   end Put_State;

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
   procedure Birdsong;
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

   procedure Revolt (Clears : Int_Arr) is

      function Count_Sym return Natural is
         Count : Natural := 0;
      begin
         for C of Clears loop
            if Map_Sympathy (C) then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Count_Sym;

      Num_Sym : constant Natural := Count_Sym;
      Clear : Natural;
      Opts  : String_Arr (1 .. Num_Sym);
      Idx   : Positive := 1;
   begin
      if Num_Sym = 0 then
         return;
      end if;

      for C of Clears loop
         if Map_Sympathy (C) then
            Opts (Idx) := Unbounded (C'Image);
            Idx := Idx + 1;
         end if;
      end loop;

      Prompt (Curr_Phase);
      Put_Line ("Which clearing has the most enemy pieces?");

      Clear := Character'Pos (Get_Option (Opts)) - Character'Pos ('a') + 1;
      Clear := Clears (Clear);
      Prompt (Curr_Phase);
      Put_Line ("Remove all enemy pieces from clearing" & Clear'Image &
                ".");
      Put_Line ("Place the" & Curr_Order'Image & " fort in clearing" &
                Clear'Image & ".");
      Forts (Clear) := Forts (Clear) + 1;
      Fort_Supply (Curr_Order) := Fort_Supply (Curr_Order) - 1;
      Continue;
   end Revolt;

   procedure Spread_Sympathy is
      Adj_Clears : array (Priority'Range) of Boolean := (others => False);
      Count      : Natural := 0;

      function Unsym_Order (Clear : Priority) return Boolean is
         (Clearings (Clear).C_Suit = Curr_Order and then Adj_Clears (Clear));
   begin
      for P in Map_Sympathy'Range loop
         if Map_Sympathy (P) then
            for C of Clearings (P).Neighbors loop
               exit when C = 0;
               Adj_Clears (C) := Adj_Clears (C) or else not Map_Sympathy (C);
            end loop;
         end if;
      end loop;

      -- Cannot spread sympathy --
      Prompt (Curr_Phase);
      if (for all C of Adj_Clears => not C) then
         Put_Line ("Cannot spread sympathy. Score +5 points for the " &
                   Name & ".");
         return;
      end if;

      -- Try ordered clearings --
      --  TODO: Account for one clearing
      Put_Line ("Which clearing has the least enemy pieces?");
      if (for some C in Adj_Clears'Range => Unsym_Order (C)) then
         for C in Adj_Clears'Range loop
            Count := Count + (if Unsym_Order (C) then 1 else 0);
         end loop;

         --  TODO Sympathy

      -- Try all clearings --
      else
         for C of Adj_Clears loop
            Count := Count + (if C then 1 else 0);
         end loop;

         --  TODO Sympathy

      end if;
   end Spread_Sympathy;

   procedure Birdsong is
      Clears : constant Int_Arr := Filter_Clearings (Curr_Order);
   begin
      Curr_Phase := Birdsong;

      Prompt (Birdsong);
      Put_Line ("Craft the order card for +1 points for the ." & Name & ".");
      Continue;

      -- Revolt --
      if Curr_Order /= Bird and then Fort_Supply (Curr_Order) > 0 then
         Revolt (Clears);

      -- Public Pity --
      else
         if Sympathy_Supply >= SYMPATHY_MAX - 4 then
            Spread_Sympathy;
         end if;
         Spread_Sympathy;
      end if;

   end Birdsong;

end Root.Alliance;

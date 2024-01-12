-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                         ROOT . ALLIANCE (Body)                            --
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
   procedure Daylight;
   procedure Evening;

   procedure Take_Turn (Order : Suit) is
   begin
      Curr_Order := Order;

      declare
         Lost : constant Natural :=
            Check_Warriors (Prompt'Access, Warrior_Supply,
                            Map_Warriors, WARRIOR_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      declare
         Lost : constant Natural :=
            Check_Buildings (Prompt'Access, Fort_Supply, Forts, FORTS_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      Check_Tokens (Prompt'Access, Sympathy_Supply, Map_Sympathy);

      Check_Rule (Prompt'Access, Rule);

      Birdsong;

      Daylight;

      Evening;

   end Take_Turn;

   function Revolt (Clears : Int_Arr) return Boolean;
   procedure Spread_Sympathy;

   procedure Birdsong is
      Clears : constant Int_Arr := Filter_Clearings (Curr_Order);
   begin
      Curr_Phase := Birdsong;

      Prompt (Birdsong);
      Put_Line ("Craft the order card for +1 points for the " & Name & ".");
      Continue;

      -- Revolt --
      if Curr_Order = Bird or else not Revolt (Clears) then

         if Sympathy_Supply >= 6 then
            Spread_Sympathy;
         end if;

         Spread_Sympathy;
      end if;

   end Birdsong;

   procedure Daylight is
      Clears : constant Int_Arr := Filter_Clearings (Bird);
      B      : Boolean;
      pragma Unreferenced (B);
   begin
      Curr_Phase := Daylight;

      Spread_Sympathy;

      if Curr_Order = Bird then
         B := Revolt (Clears);
      end if;
   end Daylight;

   procedure Organize;
   procedure Recruit;

   procedure Evening is
   begin
      Curr_Phase := Evening;

      Organize;
      Recruit;
   end Evening;

   function Revolt (Clears : Int_Arr) return Boolean is

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
      if Curr_Order = Bird then
         if (for some F of Fort_Supply => F = 0) then
            return False;
         end if;
      elsif Fort_Supply (Curr_Order) = 0 or else Num_Sym = 0 then
         return False;
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
      Put_Line ("Place the " &
                To_String (Suit_Str (Clearings (Clear).C_Suit)) &
                " Fort in clearing" & Clear'Image & ".");
      Forts (Clear) := Forts (Clear) + 1;
      Fort_Supply (Clearings (Clear).C_Suit) :=
                                    Fort_Supply (Clearings (Clear).C_Suit) - 1;
      Continue;

      return True;
   end Revolt;

   procedure Spread_Sympathy is
      Adj_Clears : array (Priority'Range) of Boolean := (others => False);
      Count      : Natural := 0;
      Clear      : Priority;

      function Unsym_Order (Clear : Priority) return Boolean is
         (Clearings (Clear).C_Suit = Curr_Order and then Adj_Clears (Clear));

      procedure Score_Sympathy is
      begin
         if Sympathy_Supply >= 9 then
            return;
         end if;

         Prompt (Curr_Phase);
         Put_Line ("Are there three or more warriors from one enemy in " &
                   "clearing" & Clear'Image & "?");

         if Get_Yes_No then
            Put ("Score +");
            Put ((case Sympathy_Supply is
                     when 0      => "3",
                     when 1 .. 2 => "2",
                     when 3 .. 4 => "1",
                     when others => "0"));
            Put_Line (" points for the " & Name & ".");
         else
            Put ("Score +");
            Put ((case Sympathy_Supply is
                     when 0      => "4",
                     when 1 .. 2 => "3",
                     when 3 .. 4 => "2",
                     when 5 .. 8 => "1",
                     when others => "0"));
            Put_Line (" points for the " & Name & ".");
         end if;
      end Score_Sympathy;

   begin
      Prompt (Curr_Phase);

      -- Cannot spread sympathy --
      if Sympathy_Supply = 0 then
         Put_Line ("Cannot spread sympathy. Score +5 points for the " &
                   Name & ".");
         Continue;
         return;
      end if;

      for P in Map_Sympathy'Range loop
         if Map_Sympathy (P) then
            for C of Clearings (P).Neighbors loop
               exit when C = 0;
               Adj_Clears (C) := Adj_Clears (C) or else not Map_Sympathy (C);
            end loop;
         end if;
      end loop;

      -- Try ordered clearings --
      if (for some C in Adj_Clears'Range => Unsym_Order (C)) then
         for C in Adj_Clears'Range loop
            Count := Count + (if Unsym_Order (C) then 1 else 0);
         end loop;

         -- Just one clearing --
         if Count = 1 then
            for C in Adj_Clears'Range loop
               if Unsym_Order (C) then
                  Clear := C;
                  exit;
               end if;
            end loop;

         -- More than one clearing --
         else
            declare
               F_Clears : Int_Arr (1 .. Count);
               Opts     : String_Arr (1 .. Count);
               Idx      : Positive := 1;
            begin
               for C in Adj_Clears'Range loop
                  if Unsym_Order (C) then
                     F_Clears (Idx) := C;
                     Opts (Idx) := Unbounded (C'Image);
                     Idx := Idx + 1;
                  end if;
               end loop;

               Put_Line ("Which clearing has the least enemy pieces?");
               Clear := Character'Pos (Get_Option (Opts)) -
                                                      Character'Pos ('a') + 1;
               Clear := F_Clears (Clear);
            end;
         end if;

      -- Try all clearings --
      else
         for C of Map_Sympathy loop
            Count := Count + (if not C then 1 else 0);
         end loop;

         -- Just one clearing --
         if Count = 1 then
            for C in Map_Sympathy'Range loop
               if not Map_Sympathy (C) then
                  Clear := C;
                  exit;
               end if;
            end loop;

         -- More than one clearing --
         else
            declare
               F_Clears : Int_Arr (1 .. Count);
               Opts     : String_Arr (1 .. Count);
               Idx      : Positive := 1;
            begin
               for C in Map_Sympathy'Range loop
                  if not Map_Sympathy (C) then
                     F_Clears (Idx) := C;
                     Opts (Idx) := Unbounded (C'Image);
                     Idx := Idx + 1;
                  end if;
               end loop;

               Put_Line ("Which clearing has the least enemy pieces?");
               Clear := Character'Pos (Get_Option (Opts)) -
                                                      Character'Pos ('a') + 1;
               Clear := F_Clears (Clear);
            end;
         end if;
      end if;

      Prompt (Curr_Phase);
      Map_Sympathy (Clear) := True;
      Sympathy_Supply := Sympathy_Supply - 1;
      Put_Line ("Place a sympathy token in clearing" & Clear'Image & ".");
      Score_Sympathy;
      Continue;
   end Spread_Sympathy;

   procedure Organize is
   begin
      for C in Priority'Range loop
         if Forts (C) = 1 and then Map_Warriors (C) >= 3 then
            Prompt (Curr_Phase);
            Put_Line ("Remove all " & Name & " warriors from clearing" &
                       C'Image & ".");
            Warrior_Supply := Warrior_Supply + Map_Warriors (C);
            Map_Warriors (C) := 0;
            Continue;

            Spread_Sympathy;
         end if;
      end loop;
   end Organize;

   procedure Recruit is
   begin
      for C in Priority'Range loop
         if Forts (C) = 1 then
            Prompt (Curr_Phase);
            Deploy_Warriors (Warrior_Supply, Map_Warriors, C, 1);
            Continue;
         end if;
      end loop;
   end Recruit;

end Root.Alliance;

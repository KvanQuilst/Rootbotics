-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . MARQUISE (Body)                           --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the Mechnical Marquise 2.0       --
-- faction from Root: The Clockwork Expansion for use in The Rootbotics      --
-- Assistant.                                                                --
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
with Ada.Containers; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Root.Marquise is

   procedure Put_Logo is
      Length : constant := 21;
   begin
      Set_Style (Yellow);
      Put_Line_Centered ("      / \     / \      ");
      Put_Line_Centered ("    / /\  \ /  /\ \    ");
      Put_Line_Centered ("  /  /  \  ^  /  \  \  ");
      Put_Line_Centered (" |   __        __    | ");
      Put_Line_Centered (" | / __ \    / __ \  | ");
      Put_Line_Centered ("_|| |__| |  | |__| | |_");
      Put_Line_Centered ("\  \ __ /    \ __ /   /");
      Put_Line_Centered ("\    ..   /\   ..     /");
      Reset_Style;
      Put (To_String (((WIDTH - Length) / 2 + 2) * "-"));
      Set_Style (Yellow);
      Put                 ("\_________________/");
      Reset_Style;
      Put_Line (To_String (((WIDTH - Length) / 2 - 1) * "-"));
   end Put_Logo;

   -------------------
   -- Faction Setup --
   -------------------

   procedure Setup (M : Map_Old) is
      Corner : Integer range 1 .. 4;
   begin
      Put_Line ("Which corner clearing will the " & Name & " start in: ");
      Corner := Get_Integer (1, 4);

      -- Place starting pieces --
      for I in Meeples'Range loop
         Meeples (I) := 1;
      end loop;
      Meeples (Corner) := Meeples (Corner) + 1;

      if M.Name = Lake then
         case Corner is
            when 1 => Meeples (2) := 0;
            when 2 => Meeples (1) := 0;
            when 3 => Meeples (4) := 0;
            when 4 => Meeples (3) := 0;
         end case;
      else
         case Corner is
            when 1 => Meeples (3) := 0;
            when 2 => Meeples (4) := 0;
            when 3 => Meeples (1) := 0;
            when 4 => Meeples (2) := 0;
         end case;
      end if;

      Building_Supply := (others => BUILDINGS_MAX - 1);
      Buildings := (others => (others => 0));

      declare
         Clearing : Priority;
         Placed   : array (Building'Range)
           of Integer range 0 .. 12 := (others => 0);
         Break : Boolean;
      begin
         for I in Building'Range loop
            Outer :
               loop
                  Put_Line ("In which clearing is the starting " &
                            I'Image & " placed?");
                  Clearing := Get_Integer (Priority'First, Priority'Last);

                  Break := Clearing = Corner;

                  for J in M.Clearings (Corner).Neighbors'Range loop
                     Break := (if Clearing = M.Clearings (Corner).Neighbors (J)
                               then True else Break);
                  end loop;

                  for J in Placed'Range loop
                     Break := (if Placed (J) = Clearing then False else Break);
                  end loop;

                  exit Outer when Break;

                  Put_Line ("Invalid response!");
                  New_Line;
               end loop Outer;
            New_Line;

            Placed (I) := Clearing;
            Buildings (I, Clearing) := 1;
         end loop;
      end;

   end Setup;

   ---------------
   -- Take Turn --
   ---------------
   procedure Pieces_Lost;
   procedure Battle  (S : Suit; M : Map_Old);
   procedure Recruit (S : Suit; M : Map_Old);
   function  Build   (S : Suit; M : Map_Old) return Boolean;
   procedure Move    (S : Suit; M : Map_Old);

   procedure Take_Turn (Order : Suit; M : Map_Old) is
      Curr_Order : Suit := Order;
      Expand : Boolean;
   begin
      for I in Rule'Range loop
         Rule (I) := False;
      end loop;

      -- Mechanical Marquise 2.0 State --
      Put_Logo;
      New_Line;
      Put_Line_Centered (Name);
      New_Line;
      Put_Line ("    Meeple Supply:" & Meeple_Supply'Image);
      Put_Line ("   Sawmill Supply:" & Building_Supply (Sawmill)'Image);
      Put_Line ("  Workshop Supply:" & Building_Supply (Workshop)'Image);
      Put_Line (" Recruiter Supply:" & Building_Supply (Recruiter)'Image);
      New_Line;
      Put (" Current Order: ");
      case Curr_Order is
         when Fox    => Put_Line (Root.IO.Fox);
         when Mouse  => Put_Line (Root.IO.Mouse);
         when Rabbit => Put_Line (Root.IO.Rabbit);
         when Bird   => Put_Line (Root.IO.Bird);
      end case;
      New_Line;
      Separator;
      New_Line;

      Pieces_Lost;

      -- Have the marquise lost? --
      if Meeple_Supply = MEEPLE_MAX then
         New_Line;
         Put_Line ("The " & Name & " cannot do anything!");
         return;
      end if;

      -- Birdsong --
      Put_Birdsong;
      Put_Line ("Craft order card for (+ 1) if it has an available item.");
      Continue;

      -- Daylight --
      Put_Daylight;

      loop

         -- Battle --
         Put_Line ("--  Battle");
         Battle (Curr_Order, M);
         Continue;

         -- Recruit --
         Put_Line ("--  Recruit");
         Recruit (Curr_Order, M);
         Continue;

         -- Build --
         Put_Line ("--  Build");
         Expand := not Build (Curr_Order, M);
         Continue;

         -- Move --
         Put_Line ("--  Move");
         Move (Curr_Order, M);
         Continue;

         exit when not Expand;

         Put_Line ("No buildings were placed, the " &
                   Name & " expand!");

         Put_Line ("What is the new order for the " & Name & "?");
         Curr_Order := Get_Suit_Opt;
         New_Line;

         New_Line;

      end loop;

      -- Evening --
      Put_Evening;
      Put ("Score  (+");
      declare
         P : Integer;
      begin
         if Order = Bird then
            -- Find building track with least remaining buildings --
            P := (if Building_Supply (Sawmill) < Building_Supply (Workshop)
                  then Building_Supply (Sawmill)
                  else Building_Supply (Workshop));
            P := (if P < Building_Supply (Recruiter)
                  then P
                  else Building_Supply (Recruiter));

            -- Score for that track --
            P := (if P /= 6 then 6 - P - 1 else 0);
         else
            P := (
              if Building_Supply (Building'Val (Suit'Pos (Order))) /= 6
              then 6 - Building_Supply (Building'Val (Suit'Pos (Order))) - 1
              else 0
              );
         end if;
         Put (P'Image);
      end;
      Put_Line (") points for the " & Name);

   end Take_Turn;

   function Check_Rule (Clearing : Priority) return Boolean is
   begin
      if Rule (Clearing) then
         return True;
      end if;

      Put_Line ("Do the " & Name & " rule clearing" & Clearing'Image & "?");
      if Get_Yes_No then
         Rule (Clearing) := True;
      end if;

      return Rule (Clearing);
   end Check_Rule;

   procedure Pieces_Lost is
   begin
      -- Determine lost warriors --
      for I in Priority'Range loop
         if Meeples (I) > 0 then
            Put_Line ("Do the " & Name & " still have" & Meeples (I)'Image &
                      " warrior(s) in clearing" & I'Image & "?");
            if not Get_Yes_No then
               Put_Line ("How many warriors remain?");
               declare
                  Val : Integer;
               begin
                  Val := Get_Integer (0, Meeples (I));
                  Meeple_Supply := Meeple_Supply + (Meeples (I) - Val);
                  Meeples (I) := Val;
               end;
            end if;
         end if;
      end loop;

      -- Check Buildings --
      for J in Building'Range loop
         for I in Priority'Range loop
            if Buildings (J, I) > 0 then
               Put_Line ("Do the " & Name & " still have" &
                         Buildings (J, I)'Image & " " &
                         J'Image & "(s) " & "in cleraing" & I'Image & "?");
               if not Get_Yes_No then
                  Put_Line ("How many " & J'Image & "s remain?");
                  declare
                     Val : Integer;
                  begin
                     Val := Get_Integer (0, Buildings (J, I));
                     Buildings (J, I) := Val;
                  end;
               end if;
            end if;
         end loop;
      end loop;
   end Pieces_Lost;

   -- Battle in each ordered clearing --
   procedure Battle (S : Suit; M : Map_Old) is
      Lost : Integer;
   begin
      for I in Priority'Range loop
         -- Check matching clearing or escalation --
         if (M.Clearings (I).C_Suit = S or else S = Bird) and then
            Meeples (I) > 0
         then
            Put_Line ("Battle in clearing" & I'Image & " the enemy with " &
                      "the most pieces, then the most points.");
            Put_Line ("How many pieces were lost?");
            Lost := Get_Integer (0, Meeples (I));
            Meeple_Supply := Meeple_Supply + Lost;
            Meeples (I) := Meeples (I) - Lost;
            New_Line;
         end if;
      end loop;
   end Battle;

   -- RECRUIT four warriors evenly among ordered clearings you rule --
   procedure Recruit (S : Suit; M : Map_Old) is
      Rule : array (Integer range 1 .. 4) of Integer := (others => 0);
      Count  : Integer range 0 .. 4 := 0;
   begin
      if S /= Bird then
         for I in Priority'Range loop
            if M.Clearings (I).C_Suit = S and then
               Meeples (I) > 0            and then
               Check_Rule (I)
            then
               Count := Count + 1;
               Rule (Count) := I;
            end if;
         end loop;
      else -- Escalation --
         for I in reverse Priority'Range loop
            if Count < 2       and then
               Meeples (I) > 0 and then
               Check_Rule (I)
            then
               Count := Count + 1;
               Rule (Count) := I;
            end if;
         end loop;
      end if;
      New_Line;

      case Count is
         when 0 =>
            Put_Line ("The " & Name & " cannot place any warriors!");
         when 1 =>
            Put_Line ("Place 4 warriors in clearing" & Rule (1)'Image);
            Meeples (Rule (1)) := Meeples (Rule (1)) + 4;
         when 2 =>
            Put_Line ("Place 2 warriors in clearing" & Rule (1)'Image);
            Put_Line ("Place 2 warriors in clearing" & Rule (2)'Image);
            Meeples (Rule (1)) := Meeples (Rule (1)) + 2;
            Meeples (Rule (2)) := Meeples (Rule (2)) + 2;
         when 3 =>
            Put_Line ("Place 2 warriors in clearing" & Rule (1)'Image);
            Put_Line ("Place 1 warrior in clearing" & Rule (2)'Image);
            Put_Line ("Place 1 warrior in clearing" & Rule (3)'Image);
            Meeples (Rule (1)) := Meeples (Rule (1)) + 2;
            Meeples (Rule (2)) := Meeples (Rule (2)) + 1;
            Meeples (Rule (3)) := Meeples (Rule (3)) + 1;
         when 4 =>
            for I in Rule'Range loop
               Put_Line ("Place 1 warrior in clearing" & Rule (I)'Image);
               Meeples (Rule (I)) := Meeples (Rule (I)) + 1;
            end loop;
      end case;

      if Count /= 0 then
         Meeple_Supply :=
           (if Meeple_Supply - 4 > 0 then Meeple_Supply - 4 else 0);
      end if;

   end Recruit;

   function Building_Space (Clearing : Priority;
                            M : Map_Old) return Boolean is
      Count : Integer := 0;
   begin
      for I in Building'Range loop
         Count := Count + Buildings (I, Clearing);
      end loop;
      return Count < M.Clearings (Clearing).Buildings;
   end Building_Space;

   -- BUILD a building the clearing you rule --
   -- with the most Marquise warriors        --
   function Build (S : Suit; M : Map_Old) return Boolean is
      Max_Idx : Integer := 0;
   begin

      for I in reverse Priority'Range loop
         -- Check matching clearing or escalation --
         if (M.Clearings (I).C_Suit = S or else S = Bird) and then
            Meeples (I) > 0 and then Check_Rule (I) and then
            Building_Space (I, M)
         then
            Put_Line ("Are there available building slots in clearing" &
              I'Image & "?");
            if Get_Yes_No then
               Max_Idx := I;
               exit;
            end if;
         end if;
      end loop;
      New_Line;

      if Max_Idx = 0 then
         Put_Line ("The " & Name & " cannot place any buildings.");
         return False;
      end if;

      -- Normal --
      if S /= Bird then
         declare
            B : constant Building := Building'Val (Suit'Pos (S));
         begin
            if Building_Supply (B) /= 0 then
               Put ("Place a " & B'Image &
                    " in clearing" & Max_Idx'Image & ".");
               Building_Supply (B) := Building_Supply (B) - 1;
               Buildings (B, Max_Idx) := Buildings (B, Max_Idx) + 1;
            else
               Put_Line ("The " & Name & " cannot place any buildings.");
               return False;
            end if;
         end;

      -- Escalation --
      else
         if Building_Supply (Sawmill) <= Building_Supply (Workshop) and then
            Building_Supply (Sawmill) <= Building_Supply (Recruiter)
         then
            if Building_Supply (Sawmill) > 0 then
               Put_Line ("Place a SAWMILL in clearing" & Max_Idx'Image);
               Building_Supply (Sawmill) := Building_Supply (Sawmill) - 1;

               Buildings (Sawmill, Max_Idx) :=
                 Buildings (Sawmill, Max_Idx) + 1;
            end if;

         elsif Building_Supply (Recruiter) <= Building_Supply (Workshop) then
            if Building_Supply (Recruiter) > 0 then
               Put_Line ("Place a RECRUITER in clearing" & Max_Idx'Image);
               Building_Supply (Recruiter) := Building_Supply (Recruiter) - 1;

               Buildings (Recruiter, Max_Idx) :=
                 Buildings (Recruiter, Max_Idx) + 1;
            end if;

         else
            if Building_Supply (Workshop) > 0 then
               Put_Line ("Place a WORKSHOP in clearing" & Max_Idx'Image);
               Building_Supply (Workshop) := Building_Supply (Workshop) - 1;

               Buildings (Workshop, Max_Idx) :=
                 Buildings (Workshop, Max_Idx) + 1;
            end if;
         end if;
      end if;

      return True;
   end Build;

   -- MOVE all but three of the warriors from each ordered    --
   -- clearing to the adjacent clearing with the most enemies --
   procedure Move (S : Suit; M : Map_Old) is
      Options : String_Arr (1 .. Neighbor_Arr'Length);
      Count   : Integer range Neighbor_Arr'Range;
   begin

      -- Get Suit Clearings --
      for I in Priority'Range loop
         -- Check matching clearing or escalation --
         if (M.Clearings (I).C_Suit = S or else S = Bird) and then
            Meeples (I) > 3
         then
            Count := 1;
            while Count < Neighbor_Arr'Length and then
                  M.Clearings (I).Neighbors (Count) /= 0
            loop
               Options (Count) :=
                 Trim (To_Unbounded_String
                 (M.Clearings (I).Neighbors (Count)'Image), Ada.Strings.Left);
               Count := Count + 1;
            end loop;

            Put_Line ("Which clearing has the most enemies?");
            declare
               Opts : constant Char_Set :=
                  Get_Options (Options (1 .. Count - 1));
               Num_Move : constant Integer := Meeples (I) - 3;
               Max : Integer := 0;
            begin
               New_Line;

               if Opts.Length = 0 then
                  Put_Line ("Move" & Num_Move'Image & " " & Name &
                            " warriors from clearing" & I'Image &
                            " to clearing" &
                            M.Clearings (I).Neighbors (1)'Image);
                  Meeples (M.Clearings (I).Neighbors (1)) :=
                    Meeples (M.Clearings (I).Neighbors (1)) + Num_Move;
                  Meeples (I) := 3;
               else
                  for C of Opts loop
                     Max := (if Character'Pos (C) - 96 > Max then
                       Character'Pos (C) - 96 else Max);
                  end loop;
                  Put_Line ("Move" & Num_Move'Image & " " & Name &
                            " warriors from clearing" & I'Image &
                            " to clearing" &
                            M.Clearings (I).Neighbors (Max)'Image);
                  Meeples (M.Clearings (I).Neighbors (Max)) :=
                    Meeples (M.Clearings (I).Neighbors (Max)) + Num_Move;
                  Meeples (I) := 3;
               end if;
               Continue;

               -- Battle if in escalation --
               if S = Bird then
                  Max := (if Max = 0 then 1 else Max); declare
                     Lost : Integer;
                  begin
                     Put_Line ("Battle in clearing" &
                               M.Clearings (I).Neighbors (Max)'Image &
                               " the enemy with the most pieces, " &
                               "then the most points.");
                     Put_Line ("How many pieces were lost?");
                     Lost := Get_Integer
                       (0, Meeples (M.Clearings (I).Neighbors (Max)));
                     Meeple_Supply := Meeple_Supply + Lost;
                     Meeples (M.Clearings (I).Neighbors (Max)) :=
                       Meeples (M.Clearings (I).Neighbors (Max)) - Lost;
                     Continue;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Move;

end Root.Marquise;

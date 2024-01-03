-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                          ROOT . LIZARDS (Body)                            --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the implementation of the Logical Lizards faction      --
-- from Root: The Clockwork Expansion 2 for use in The Rootbotics Assistant. --
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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Root.Maps; use Root.Maps;

package body Root.Lizards is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
      B_Col : constant := (WIDTH - Logo_Width) / 2 + 2;
   begin
      Put_Line_Centered (Name);
      Cursor_Line_Move (9);
      Put_Line (To_String (WIDTH * '-'));
      Cursor_Line_Move (-9);
      for L of Logo loop
         Cursor_Column_Set (B_Col);
         Put (To_String (L));
         Cursor_Line_Move (1);
      end loop;
      New_Line;
   end Put_Logo;

   procedure Put_State is
      procedure Garden_State (G : Garden) is
         Used : constant String := (GARDENS_MAX - Garden_Supply (G)) * " **";
         Remaining : constant String := Garden_Supply (G) * " **";
      begin
         Set_Style (B_Black);
         Put (Used);
         Set_Style ((case G is
                     when Fox => Red,
                     when Rabbit => B_Yellow,
                     when Mouse => Yellow));
         Put (Remaining);
         Reset_Style;
      end Garden_State;

      procedure Conspiracy_State is
      begin
         for C in Conspiracy_Count'Range loop
            if C = 3 then
               New_Line;
               Put ("                   ");
            else
               Put (" ");
            end if;
            if C = Next_Conspiracy then
               Set_Style (Green);
               Put (Conspiracies (C)'Image);
               Reset_Style;
            elsif C = Next_Conspiracy - 1 then
               Set_Style (B_Black);
               Put (Conspiracies (C)'Image);
               Reset_Style;
            else
               Put (Conspiracies (C)'Image);
            end if;
         end loop;
      end Conspiracy_State;
   begin
      Put_Line ("   Warrior Supply:" & Warrior_Supply'Image);
      Put_Line ("         Acolytes:" & Acolytes'Image);
      Put ("    " & Root.IO.Mouse & " Gardens:");
      Garden_State (Mouse);
      New_Line;

      Put ("   " & Root.IO.Rabbit & " Gardens:");
      Garden_State (Rabbit);
      New_Line;

      Put ("      " & Root.IO.Fox & " Gardens:");
      Garden_State (Fox);
      New_Line;

      Put (" Conspiracies:");
      Conspiracy_State;
      New_Line;
   end Put_State;

   procedure Prompt is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Gardens, Rule, Curr_Order);
   end Prompt;

   -------------------
   -- Lizards Setup --
   -------------------
   procedure Setup is
      Corner : Integer range 1 .. 4;
   begin
      Put_Line ("Which corner will the " & Name & " start in?");
      Corner := Get_Integer (1, 4);

      -- Starting Clearing --
      Map_Warriors (Corner) := 4;
      Warrior_Supply := Warrior_Supply - 4;
      Gardens (Corner) := 1;
      Garden_Supply (Clearings (Corner).C_Suit) := GARDENS_MAX - 1;
      Rule (Corner) := True;

      -- Adjacent Clearing Warriros --
      for C of Clearings (Corner).Neighbors loop
         exit when C = 0;
         Map_Warriors (C) := 1;
         Warrior_Supply := Warrior_Supply - 1;
      end loop;
   end Setup;

   ------------------------
   -- Lizards Turn Logic --
   ------------------------
   procedure Birdsong (Order : Suit) is
      Idle_Count : Natural := 0;
      Clears     : constant Int_Arr := Filter_Clearings (Order);
      Val        : Natural;

      procedure Convert is null;

      procedure Crusade is
      begin
         for C of Clears loop
            if Map_Warriors (C) >= 2 then
               Put_Line ("Are there enemies in clearing" &
                         C'Image & "?");
               if Get_Yes_No then
                  Put_Line ("Battle the enemy faction with the most points " &
                            "in clearing" & C'Image);
                  Put_Line ("How many warriors were lost?");
                  Val := Get_Integer (0, (if 3 > Map_Warriors (C)
                                          then Map_Warriors (C)
                                          else 3));
                  Map_Warriors (C) := Map_Warriors (C) - Val;
                  Warrior_Supply := Warrior_Supply + Val;
                  Idle_Count := 0;
               end if;
            end if;
         end loop;
      end Crusade;

      procedure Sanctify is
         Opts : String_Arr (1 .. Clears'Length);
         Opt  : Character;
      begin
         for I in Clears'Range loop
            Opts (I) := Unbounded (Clears (I)'Image);
         end loop;

         Put_Line ("Which clearing has an enemy building with the most " &
                   "points and least warriors?");
         Opt := Get_Option (Opts);

         --  TODO Finish sanctify

      end Sanctify;
   begin
      for A in 0 .. Acolytes loop
         Idle_Count := Idle_Count + 1;
         case Conspiracies (Next_Conspiracy) is
            when Convert => Convert;
            when Crusade => Crusade;
            when Sanctify => Sanctify;
         end case;
         Next_Conspiracy := Next_Conspiracy + 1;

         exit when Idle_Count = Conspiracies'Length;
      end loop;
   end Birdsong;

   procedure Take_Turn is
   begin
      Curr_Order := Bird;

      ----------------------
      -- Confirm Warriors --
      ----------------------
      Prompt;
      Put_Line ("Does the number of warriors match for each clearing?");
      if not Get_Yes_No then
         Put_Line ("Which clearings are incorrect?");
         declare
            Clearings : constant Int_Arr := Get_Integers (1, 12);
            Warriors  : Integer;
            Supply    : Integer := Warrior_Supply;
         begin
            loop
               for C of Clearings loop
                  exit when C = 0;

                  Prompt;
                  Put_Line ("What is the number of warriors in clearing" &
                             C'Image & "?");
                  Warriors := Get_Integer (0, WARRIOR_MAX);
                  Supply := (if Warriors = 0
                             then Supply + Map_Warriors (C)
                             else Supply - Warriors);
                  Map_Warriors (C) := Warriors;
               end loop;

               exit when Supply >= 0 and then Supply <= WARRIOR_MAX;

               Put_Line ("The provided values don't add up, lets try again.");
            end loop;
            if Supply > Warrior_Supply then
               Acolytes := Supply - Warrior_Supply;
            else
               Warrior_Supply := Supply;
            end if;
         end;
      end if;

      --------------
      -- Birdsong --
      --------------
      Prompt;
      if Acolytes > 0 then
         Put_Line ("Which suit most common suit in the Lost Souls pile " &
                   "(Ties go to " & Root.IO.Bird & ")?");
         Curr_Order := Get_Suit_Opt;
         Birdsong (Curr_Order);
      end if;

      --------------
      -- Daylight --
      --------------
      for I in Integer range 1 .. 4 loop
         Prompt;
         Put_Line ("Reveal top card of Lost Souls pile; what is it's suit?");
         declare
            S : constant Suit := Get_Suit_Opt;
            Max : Integer := 0;
            Max_Clear : Priority := 1;
         begin
            if S = Bird then
               for C in Map_Warriors'Range loop
                  if Map_Warriors (C) > Max then
                     Max := Map_Warriors (C);
                     Max_Clear := C;
                  end if;
               end loop;
               if Max > 0 then
                  Map_Warriors (Max_Clear) := Map_Warriors (Max_Clear) - 1;
                  Acolytes := Acolytes + 1;
                  Put_Line ("Add a warrior from clearing" & Max_Clear'Image &
                            " to the acolytes box.");
                  Put_Line ("Discard the " & Root.IO.Bird & " card.");
               else
                  Put_Line ("Nothing to do...");
               end if;
            else
               null;
            end if;
         end;
         Continue;
      end loop;

      -------------
      -- Evening --
      -------------
      Prompt;

      -- Determine scoring garden track --
      declare
         Min : Integer := GARDENS_MAX;
      begin
         for Num of Garden_Supply loop
            Min := (if Num < Min then Num else Min);
         end loop;
         case Min is
            when 0      => Put_Line ("Score 4 points for the " & Name);
            when 1      => Put_Line ("Score 3 points for the " & Name);
            when 2 | 3  => Put_Line ("Score 2 points for the " & Name);
            when others => null;
         end case;
      end;

      Put_Line ("Discard Lost Souls deck.");
      Put_Line ("Return revealed cards IN ORDER to Lost Souls.");
      Put_Line ("Craft the top card of the deck for 1 point and add it " &
                "to your Lost Souls pile.");

      Continue;

      --  TODO: Final Map Check
   end Take_Turn;

end Root.Lizards;

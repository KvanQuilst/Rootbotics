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
with Ada.Text_IO; use Ada.Text_IO;

with Root.Maps; use Root.Maps;

package body Root.Lizards is

   ---------------
   -- Prompt IO --
   ---------------
   procedure Put_Logo is
   begin
      Root.Faction.Put_Logo (Name, Logo, Logo_Width);
   end Put_Logo;

   procedure Put_State is
      procedure Garden_State (G : Garden) is
         Used : constant String :=
            To_String ((GARDENS_MAX - Garden_Supply (G)) * " **");
         Remaining : constant String :=
            To_String (Garden_Supply (G) * " **");
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
      Put ("    " & To_String (Suit_Str (Mouse)) & " Gardens:");
      Garden_State (Mouse);
      New_Line;

      Put ("   " & To_String (Suit_Str (Rabbit)) & " Gardens:");
      Garden_State (Rabbit);
      New_Line;

      Put ("      " & To_String (Suit_Str (Fox)) & " Gardens:");
      Garden_State (Fox);
      New_Line;

      Put (" Conspiracies:");
      Conspiracy_State;
      New_Line;
   end Put_State;

   procedure Put_Phase is
   begin
      Root.IO.Put_Phase (Curr_Phase,
                        (case Curr_Action is
                           when Outcasts => "Outcasts",
                           when Convert  => "Convert",
                           when Crusade  => "Crusade",
                           when Sanctify => "Sanctify",
                           when Rituals  => "Rituals",
                           when None     => ""));
   end Put_Phase;

   procedure Prompt is
   begin
      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors,
                  Gardens, Rule, Curr_Order, Put_Phase'Access);
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

   ----------------------------
   -- Lizards Common Actions --
   ----------------------------
   procedure Deploy_Building (Clear : Priority) is
      S : constant Suit := Clearings (Clear).C_Suit;
   begin
      if Garden_Supply (S) > 0 then
         Root.Faction.Deploy_Building (Garden_Supply (S), Gardens,
                                       Clear, S'Image & " Garden");
         Rule (Clear) := True;
      end if;
   end Deploy_Building;

   ------------------------
   -- Lizards Turn Logic --
   ------------------------
   procedure Take_Turn is
   begin
      Curr_Order  := Bird;
      Curr_Phase  := None;
      Curr_Action := None;

      -- Confirm Warriors --
      declare
         Lost : Natural;
      begin
         Lost := Check_Warriors (Prompt'Access, Warrior_Supply,
                                 Map_Warriors, WARRIOR_MAX);
         Acolytes := Acolytes + Lost;
         Warrior_Supply := Warrior_Supply - Lost;
      end;

      -- Confirm Buildings --
      declare
         Lost : constant Natural :=
            Check_Buildings (Prompt'Access, Garden_Supply,
                             Gardens, GARDENS_MAX);
         pragma Unreferenced (Lost);
      begin null; end;

      -- Confirm Rule --
      for C in Gardens'Range loop
         Rule (C) := Rule (C) or else Gardens (C) > 0;
      end loop;
      Check_Rule (Prompt'Access, Rule);

      Birdsong;

      Daylight;

      Evening;

   end Take_Turn;

   ------------
   -- Phases --
   ------------
   procedure Birdsong is
   begin
      Curr_Phase := Birdsong;
      if Acolytes > 0 then
         Outcasts;
         Do_Conspiracies;
      end if;
   end Birdsong;

   procedure Daylight is
   begin
      Curr_Phase := Daylight;
      Curr_Action := Rituals;

      for I in Integer range 1 .. 4 loop
         Prompt;
         Put_Line ("Reveal top card of Lost Souls pile; what is it's suit?");
         Ritual (Get_Suit_Opt);
         Continue;
      end loop;
   end Daylight;

   procedure Evening is
      Min : Integer := GARDENS_MAX;
   begin
      Curr_Phase := Evening;
      Curr_Action := None;
      Prompt;

      -- Determine scoring garden track --
      for Num of Garden_Supply loop
         Min := (if Num < Min then Num else Min);
      end loop;
      case Min is
         when 0      => Put_Line ("Score 4 points for the " & Name);
         when 1      => Put_Line ("Score 3 points for the " & Name);
         when 2 | 3  => Put_Line ("Score 2 points for the " & Name);
         when others => null;
      end case;

      Put_Line ("Discard Lost Souls deck.");
      Put_Line ("Return revealed cards IN ORDER to Lost Souls.");
      Put_Line ("Craft the top card of the deck for 1 point and add it " &
                "to your Lost Souls pile.");
      Continue;
   end Evening;

   -------------
   -- Actions --
   -------------
   procedure Outcasts is
   begin
      Curr_Action := Outcasts;
      Prompt;
      Put_Line ("Which suit is the most common suit in the Lost Souls pile" &
                "(Ties go to " & To_String (Suit_Str (Bird)) & ")?");
      Curr_Order := Get_Suit_Opt;
   end Outcasts;

   procedure Do_Conspiracies is
      Idle_Count : Natural := 0;
      Clears     : constant Int_Arr := Filter_Clearings (Curr_Order);
      Val        : Natural;

      procedure Convert is
         Opts : String_Arr (1 .. Clears'Length + 1);
         Clear : Priority;
      begin
         Curr_Action := Convert;
         for I in Clears'Range loop
            Opts (I) := Unbounded (Clears (I)'Image);
         end loop;
         Opts (Clears'Length + 1) := Unbounded ("None");

         Put_Line ("Which clearing has an enemy warrior with the most " &
                   "points and the most enemy buildings?");

         Clear := Character'Pos (Get_Option (Opts)) - Character'Pos ('a') + 1;
         if Clear /= Clears'Length + 1 then
            Put_Line ("Replace 1 warrior in clearing" & Clear'Image &
                      " with a warrior from the acolytes box.");
            Clear := Clears (Clear);
            Map_Warriors (Clear) := Map_Warriors (Clear) + 1;
            Idle_Count := 0;
            Continue;
         end if;
      end Convert;

      procedure Crusade is
      begin
         Curr_Action := Crusade;
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
         if Idle_Count = 0 then
            Continue;
         end if;
      end Crusade;

      procedure Sanctify is
         Opts  : String_Arr (1 .. Clears'Length);
         Clear : Priority;
      begin
         Curr_Action := Sanctify;
         for I in Clears'Range loop
            Opts (I) := Unbounded (Clears (I)'Image);
         end loop;

         Put_Line ("Which clearing has an enemy building with the most " &
                   "points and least warriors?");
         Clear := Character'Pos (Get_Option (Opts)) - Character'Pos ('a') + 1;
         if Clear /= Clears'Length + 1 then
            Clear := Clears (Clear);
            Gardens (Clear) := Gardens (Clear) + 1;
            Garden_Supply (Clearings (Clear).C_Suit) :=
               Garden_Supply (Clearings (Clear).C_Suit) - 1;
            Rule (Clear) := True;
            Idle_Count := 0;
            Continue;
         end if;
      end Sanctify;

   begin
      for A in 0 .. Acolytes loop
         Prompt;
         Idle_Count := Idle_Count + 1;
         case Conspiracies (Next_Conspiracy) is
            when Convert => Convert;
            when Crusade => Crusade;
            when Sanctify => Sanctify;
         end case;
         Next_Conspiracy := Next_Conspiracy + 1;

         exit when Idle_Count = Conspiracies'Length;
         if Idle_Count = 0 then
            Acolytes := Acolytes - 1;
         end if;
      end loop;
   end Do_Conspiracies;

   procedure Ritual (S : Suit) is
      Max       : Integer  := 0;
      Max_Clear : Priority := 1;
      Clears    : constant Int_Arr  :=  Filter_Clearings (S);
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
            Put_Line ("Discard the " & To_String (Suit_Str (Bird)) & " card.");
         else
            Put_Line ("Nothing to do...");
         end if;

      -- Fox, Rabbit, Mouse Suit --
      else
         declare
            Opts : String_Arr (1 .. Clears'Length + 1);
            Clear : Priority;
         begin
            for I in Clears'Range loop
               Opts (I) := Unbounded (Clears (I)'Image);
            end loop;
            Opts (Clears'Length + 1) := Unbounded ("None");

            Put_Line ("Which clearing has an open building slots AND the " &
                      "most enemy buildings?");

            Clear := Character'Pos (Get_Option (Opts)) -
                                                      Character'Pos ('a') + 1;
            if Clear /= Clears'Length + 1 then
               Clear := Clears (Clear);
               Deploy_Warriors (Warrior_Supply, Map_Warriors, Clear, 1);

               if Rule (Clear) or else Get_Rule (Name, Clear) then
                  Deploy_Building (Clear);
               end if;
            else
               Deploy_Warriors (Warrior_Supply, Map_Warriors, Clears (1), 1);
            end if;
         end;
      end if;
   end Ritual;

end Root.Lizards;

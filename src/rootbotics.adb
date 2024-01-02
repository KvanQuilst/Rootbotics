-------------------------------------------------------------------------------
--                                                                           --
--                           THE ROOTBOTICS TOOL                             --
--                                                                           --
--                                ROOTBOTICS                                 --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the main execution and gameplay loop logic for The     --
-- Rootbotics Tool.                                                          --
--                                                                           --
-- The Rootbotics Tool is free software: you can redistribute it and/or      --
-- modify it under the terms of the GNU General Public License as published  --
-- by the Free Software Foundation, either version 3 of the License, or (at  --
-- your option) any later version.                                           --
--                                                                           --
-- GBADA is distributed in the hope that it will be useful, but WITHOUT ANY  --
-- WARRANTY; wihtout even the implied warranty of MERCHANTABILITY or FITNESS --
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more     --
-- details.                                                                  --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Tool. If not, see <https://www.gnu.org/licenses/>.    --
-------------------------------------------------------------------------------
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

with Root.Eyrie;
with Root.Marquise;
with Root.Alliance;
with Root.Vagabot;
with Root.Lizards;

procedure Rootbotics is
   VERSION : constant String := "v0.1";

   -- In order of setup priority --
   type Faction is (Marquise, Eyrie, Alliance, Vagabot, Lizards);

   Playing : array (Faction'Range) of Boolean := (others => False);
   Num_Playing : Integer := 0;

   M : Map;

begin
   Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
   New_Line;

   -----------------------
   -- Faction Selection --
   -----------------------
   Put_Line ("Which factions will you play with?");
   declare
      Options : constant String_Arr := (
         To_Unbounded_String (Root.Marquise.Name),
         To_Unbounded_String (Root.Eyrie.Name),
         To_Unbounded_String (Root.Alliance.Name),
         To_Unbounded_String (Root.Vagabot.Name),
         To_Unbounded_String (Root.Lizards.Name)
         --  To_Unbounded_String (Root.Riverfolk.Name),
         --  To_Unbounded_String (Root.Corvids.Name),
         --  To_Unbounded_String (Root.Duchy.Name)
         );
      Opts : constant Char_Arr := Get_Options (Options);
   begin
      if Opts'Length = 0 then
         return;
      end if;

      for I in Opts'Range loop
         Playing (Faction'Val (Character'Pos (Opts (I)) - 97)) := True;
      end loop;
      Num_Playing := Opts'Length;
   end;
   New_Line;

   -------------------
   -- Map Selection --
   -------------------
   declare
      Options : constant String_Arr := (
         To_Unbounded_String (String_Style ("Fall", Green)),
         To_Unbounded_String (String_Style ("Winter", B_Cyan)),
         To_Unbounded_String (String_Style ("Lake", Blue)),
         To_Unbounded_String (String_Style ("Mountain", Yellow))
         );
      Opt : Character;
   begin
      Put_Line ("Which map will you be playing on:");
      Opt := Get_Option (Options);

      case Opt is
         when 'a' => M := Fall_Map;
         when 'b' => M := Winter_Map;
         when 'c' => M := Lake_Map;
         when 'd' => M := Mountain_Map;
         when others =>
            Put_Line ("ERROR: Should never reach here!");
         return;
      end case;
   end;
   New_Line;

   ----------------------------
   -- Faction Setup in Order --
   ----------------------------
   for I in Playing'Range loop
      if Playing (I) then
         case I is
            when Marquise => Root.Marquise.Setup (M);
            when Eyrie => Root.Eyrie.Setup;
            when Alliance =>
               Put_Line ("The Automated Alliance is unimplmented!");
            when Vagabot => Root.Vagabot.Setup;
            when Lizards => Root.Lizards.Setup;
         end case;
      end if;
   end loop;
   New_Line;

   ---------------------
   -- Manage the Game --
   ---------------------
   declare
      Options : String_Arr (1 .. Num_Playing);
      P_Idx : Integer := 0;
      F_Opt : Character;
      Order : Suit;
      F : Faction;
   begin

      for I in Playing'Range loop
         if Playing (I) then
            P_Idx := P_Idx + 1;
            case I is
               when Marquise =>
                  Options (P_Idx) := To_Unbounded_String (Root.Marquise.Name);
               when Eyrie    =>
                  Options (P_Idx) := To_Unbounded_String (Root.Eyrie.Name);
               when Alliance =>
                  Options (P_Idx) := To_Unbounded_String (Root.Alliance.Name);
               when Vagabot  =>
                  Options (P_Idx) := To_Unbounded_String (Root.Vagabot.Name);
               when Lizards =>
                  Options (P_Idx) := To_Unbounded_String (Root.Lizards.Name);
            end case;
         end if;
      end loop;
      Separator;

      loop
         if Num_Playing > 1 then
            -- Choose Faction Turn --
            Put_Line ("Whose turn will you take?");
            F_Opt := Get_Option (Options);
            P_Idx := Character'Pos (F_Opt) - 96;
         else
            New_Line;
            Put_Line ("Take the " & Root.Lizards.Name & "'s turn.");
            Continue;
            P_Idx := 1;
         end if;

         F := Lizards;
         loop
            if Playing (F) then
               P_Idx := P_Idx - 1;
            end if;

            exit when P_Idx = 0;
            F := Faction'Succ (F);
         end loop;

         New_Line;

         -- What's the Order? --
         if F /= Lizards then
            Put_Line ("What is the order of this turn?");
            Order := Get_Suit_Opt;
            New_Line;
         end if;

         -- Handle faction turn --
         case F is
            when Marquise => Root.Marquise.Take_Turn (Order, M);
            when Eyrie    => Root.Eyrie.Take_Turn (Order, M);
            when Alliance => Root.Alliance.Take_Turn (Order, M);
            when Vagabot  => Root.Vagabot.Take_Turn (Order, M);
            when Lizards  => Root.Lizards.Take_Turn;
         end case;
      end loop;
   end;
end Rootbotics;

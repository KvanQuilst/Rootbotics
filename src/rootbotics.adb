-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                                ROOTBOTICS                                 --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the main execution and gameplay loop logic for The     --
-- Rootbotics Assistant.                                                     --
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

with Root; use Root;
with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

with Root.Alliance;
with Root.Lizards;
with Root.Duchy;

procedure Rootbotics is
   VERSION : constant String := "v0.3-dev";

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;

   -- In order of setup priority --
   type Faction is (Alliance, Lizards, Duchy);
   Name      : constant array (Faction'Range) of Unbounded_String :=
      (Unbounded (Root.Alliance.Name_Plain),
       Unbounded (Root.Lizards.Name_Plain),
       Unbounded (Root.Duchy.Name_Plain));
   Setup     : constant array (Faction'Range) of access procedure :=
      (Root.Alliance.Setup'Access,
       Root.Lizards.Setup'Access,
       Root.Duchy.Setup'Access);
   Take_Turn : constant array (Faction'Range) of access procedure :=
      (Root.Alliance.Take_Turn'Access,
       Root.Lizards.Take_Turn'Access,
       Root.Duchy.Take_Turn'Access);

   Playing : array (Faction'Range) of Boolean := (others => False);
   Num_Playing : Integer := 0;
begin
   Put_Title_Prompt;
   Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
   New_Line;

   -----------------------
   -- Faction Selection --
   -----------------------
   Put_Line ("Which factions will you play with?");
   declare
      Opts : constant Char_Set := Get_Options_HL
         ((Name (Alliance),
           Name (Lizards),
           Name (Duchy)),
          (Root.Alliance.Faction_Color,
           Root.Lizards.Faction_Color,
           Root.Duchy.Faction_Color));
   begin
      if Opts.Length = 0 then
         return;
      end if;

      for C of Opts loop
         Playing (Faction'Val (Character'Pos (C) - 97)) := True;
      end loop;
      Num_Playing := Integer (Opts.Length);
   end;

   -------------------
   -- Map Selection --
   -------------------
   Put_Title_Prompt;
   Put_Line ("Which map will you be playing on:");
   declare
      Opt : constant Character :=
         Get_Option_HL ((Unbounded ("Fall"),
                         Unbounded ("Winter"),
                         Unbounded ("Lake"),
                         Unbounded ("Mountain")),
                        (Green,
                         B_Cyan,
                         Blue,
                         Yellow));
   begin
      Init_Map (case Opt is
                  when 'a' => Fall,
                  when 'b' => Winter,
                  when 'c' => Lake,
                  when 'd' => Mountain,
                  when others => Fall);
   end;
   New_Line;

   ----------------------------
   -- Faction Setup in Order --
   ----------------------------
   for F in Faction'Range loop
      if Playing (F) then
         Setup (F).all;
      end if;
   end loop;
   New_Line;

   ---------------------
   -- Manage the Game --
   ---------------------

   -- There's one faction --
   if Num_Playing = 1 then
      declare
         Fact : Faction;
      begin
         for F in Faction'Range loop
            Fact := F;
            exit when Playing (F);
         end loop;

         Put_Title_Prompt;
         Put_Line ("Take the " & To_String (Name (Fact)) & "'s turn.");
         Continue;

         loop
            Take_Turn (Fact).all;
         end loop;
      end;
   end if;

   -- There's multiple factions --
   declare
      Options : String_Arr (1 .. Num_Playing);
      P_Idx : Integer := 0;
      F_Opt : Character;
   begin

      for F in Faction'Range loop
         if Playing (F) then
            P_Idx := P_Idx + 1;
            Options (P_Idx) := Name (F);
         end if;
      end loop;
      Separator;

      loop
         --  TODO: Grey out factions who've already had a turn this round

         -- Choose Faction Turn --
         Put_Title_Prompt;
         Put_Line ("Whose turn will you take?");
         F_Opt := Get_Option (Options);

         case F_Opt is
            when 'a' => Take_Turn (Alliance).all;
            when 'b' => Take_Turn (Lizards).all;
            when 'c' => Take_Turn (Duchy).all;
            when others => return;
         end case;
      end loop;
   end;
end Rootbotics;

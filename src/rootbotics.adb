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

   -- In order of setup priority --
   type Faction is (Alliance, Lizards, Duchy);

   Playing : array (Faction'Range) of Boolean := (others => False);
   Num_Playing : Integer := 0;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;
begin
   Put_Title_Prompt;
   Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
   New_Line;

   -----------------------
   -- Faction Selection --
   -----------------------
   Put_Line ("Which factions will you play with?");
   declare
      Options : constant String_Arr :=
         (Unbounded (Root.Alliance.Name),
          Unbounded (Root.Lizards.Name),
          Unbounded (Root.Duchy.Name));
      Opts : constant Char_Set := Get_Options (Options);
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
   declare
      Options : constant String_Arr :=
         (Unbounded (String_Style ("Fall", Green)),
          Unbounded (String_Style ("Winter", B_Cyan)),
          Unbounded (String_Style ("Lake", Blue)),
          Unbounded (String_Style ("Mountain", Yellow)));
   begin
      Put_Title_Prompt;
      Put_Line ("Which map will you be playing on:");
      Init_Map (case Get_Option (Options) is
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
         case F is
            when Alliance  => Root.Alliance.Setup;
            when Lizards   => Root.Lizards.Setup;
            when Duchy     => Root.Duchy.Setup;
         end case;
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
         Put_Line ("Take the " &
                   (case Fact is
                     when Alliance  => Root.Alliance.Name,
                     when Lizards   => Root.Lizards.Name,
                     when Duchy     => Root.Duchy.Name) &
                   "'s turn.");
         Continue;

         loop
            case Fact is
               when Alliance  => Root.Alliance.Take_Turn;
               when Lizards   => Root.Lizards.Take_Turn;
               when Duchy     => Root.Duchy.Take_Turn;
            end case;
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
            case F is
               when Alliance =>
                  Options (P_Idx) := Unbounded (Root.Alliance.Name);
               when Lizards  =>
                  Options (P_Idx) := Unbounded (Root.Lizards.Name);
               when Duchy    =>
                  Options (P_Idx) := Unbounded (Root.Duchy.Name);
            end case;
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
            when 'a' => Root.Alliance.Take_Turn;
            when 'b' => Root.Lizards.Take_Turn;
            when 'c' => Root.Duchy.Take_Turn;
            when others => return;
         end case;
      end loop;
   end;
end Rootbotics;

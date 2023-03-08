with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Root.Alliance is

   procedure Put_Logo is
      Length : constant := 16;
   begin
      Set_Style (Green);
      Put_Line_Centered ("      _       _      ");
      Put_Line_Centered ("     / \     / \     ");
      Put_Line_Centered ("    |/ \|   |/ \|    ");
      Put_Line_Centered ("    || ||   || ||    ");
      Put_Line_Centered ("   _|| ||___|| ||_   ");
      Put_Line_Centered ("  /  __       __   \ ");
      Put_Line_Centered (" / /   /\   /   /\  \");
      Put_Line_Centered ("| |   <  | |   <  | |");
      Put_Line_Centered ("|  \ __\/   \ __\/  |");
      Put_Line_Centered (" \  . .   |    . .  /");
      Reset_Style;
      Put (To_String ((WIDTH - Length) / 2 * "-"));
      Set_Style (Green);
      Put               ("\________________/");
      Reset_Style;
      Put_Line (To_String ((WIDTH - Length) / 2 * "-"));
   end Put_Logo;

   -------------------
   -- Faction Setup --
   -------------------

   function Setup (Diff : Difficulty) return Boolean is
   begin
      return True;
   end Setup;

   ---------------
   -- Take Turn --
   ---------------

   procedure Take_Turn (Order : Suit; M : Map) is
   begin

      -- Alliance State --
      Put_Logo;
      New_Line;
      Set_Style (Green);
      Put_Line_Centered ("Automated Alliance");
      Reset_Style;
      -- F  M  R --
      -- X  X  X --
      -- Sympathetic Clearings: X --
      New_Line;
      Separator;

      -- Birdsong --
      Put_Birdsong;

      Continue;

      -- Daylight --
      Put_Daylight;

      Continue;

      -- Evening --
      Put_Evening;

      Continue;

   end Take_Turn;

end Root.Alliance;

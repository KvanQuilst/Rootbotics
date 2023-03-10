with Ada.Text_IO; use Ada.Text_IO;
with Root.IO; use Root.IO;

package body Root.Marquise.Help is

   procedure Build is
   begin
      New_Line;
      Separator;
      Put_Line_Centered (Name);
      New_Line;
      Put_Line ("Build:");
      Put_Line ("   Build a building in the clearing you rule");
      Put_Line ("   with the most Marquise warriors. Place a");
      Put_Line ("   sawmill for " & Root.IO.Fox & " order, a workshop for");
      Put_Line ("   " & Root.IO.Rabbit & " order, or a recruiter for " &
                Root.IO.Mouse & ".");
      New_Line;
      Separator;
      New_Line;
   end Build;

   procedure Escalated_Build is
   begin
      New_Line;
      Separator;
      Put_Line_Centered (Name);
      New_Line;
      Put_Line ("Build (Escalated Daylight):");
      Put_Line ("   Build a building of the type with the most");
      Put_Line ("   pieces on the map in a clearing you rule");
      Put_Line ("   with the most Marquise warriors.");
      Put_Line ("   * On tie between sawmills and any other");
      Put_Line ("     building types, place a sawmill.");
      Put_Line ("   * On tie between workshops and recruiters");
      Put_Line ("     but not sawmills, place a recruiter.");
      New_Line;
      Separator;
      New_Line;
   end Escalated_Build;

   procedure Move is
   begin
      New_Line;
      Separator;
      Put_Line_Centered (Name);
      New_Line;
      Put_Line ("Move:");
      Put_Line ("   Move all but three warriors from each ordered");
      Put_Line ("   clearing to adjacent clearing with the most");
      Put_Line ("   enemy pieces.");
      New_Line;
      Separator;
      New_Line;
   end Move;

   procedure Escalated_Move is
   begin
      New_Line;
      Separator;
      Put_Line_Centered (Name);
      New_Line;
      Put_Line ("Move (Escalated Daylight):");
      Put_Line ("   Move all but three warriors from each clearing");
      Put_Line ("   to the adjacent clearing with the most enemy");
      Put_Line ("   pieces. Then battle in each clearing you moved");
      Put_Line ("   into");
      New_Line;
      Separator;
      New_Line;
   end Escalated_Move;

end Root.Marquise.Help;

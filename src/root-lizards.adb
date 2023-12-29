with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Root.Lizards is

   procedure Put_Logo is
      Length : constant := 21;
   begin
      --  Put_Line_Centered ("TO BE DESIGNED");
      Put_Line_Centered (Name);
      New_Line;
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
      Put_Line (To_String (((WIDTH - Length) / 2 + 1) * "-"));
   end Put_Logo;

   procedure Prompt is
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
   begin
      Erase_Screen;
      Cursor_Home;
      Put_Logo;
      Put_Map (Fall, Meeples);
      New_Line;
      Separator;

      -- Lizards State --
      Put_Line ("    Meeple Supply:" & Meeple_Supply'Image);
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

      New_Line;
      Put ("    Current Order: ");
      Put_Line ((case Curr_Order is
                 when Fox => Root.IO.Fox,
                 when Rabbit => Root.IO.Rabbit,
                 when Mouse => Root.IO.Mouse,
                 when Bird => Root.IO.Bird));

      Separator;
   end Prompt;

   procedure Take_Turn (M : Map) is
   begin
      for I in Rule'Range loop
         Rule (I) := False;
      end loop;

      Curr_Order := Fox;

      Prompt;

   end Take_Turn;

end Root.Lizards;

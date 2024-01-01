with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

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

   procedure Setup is
   begin
      null;
   end Setup;

   procedure Take_Turn (M : Map) is
   begin
      for I in Rule'Range loop
         Rule (I) := False;
      end loop;

      Curr_Order := Fox;

      Put_Prompt (Put_Logo'Access, Put_State'Access, Map_Warriors, Fox);

   end Take_Turn;

end Root.Lizards;

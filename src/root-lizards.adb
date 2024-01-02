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
   procedure Take_Turn is
      Outcasts : constant String_Arr := (Unbounded (Root.IO.Fox),
                                         Unbounded (Root.IO.Rabbit),
                                         Unbounded (Root.IO.Mouse));
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
            Warrior_Supply := Supply;
         end;
      end if;

      --------------
      -- Birdsong --
      --------------
      if Acolytes > 0 then
         Put_Line ("Which suit most common suit in the Lost Souls pile?");
         Curr_Order := (case Get_Option (Outcasts) is
                           when 'a' => Fox,
                           when 'b' => Rabbit,
                           when 'c' => Mouse,
                           when others => Bird);
      end if;

   end Take_Turn;

end Root.Lizards;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Root.IO; use Root.IO;

package body Root.Maps is
   package Int_IO is new Integer_IO (Integer);

   function Clearings return Clearing_Arr is
      (case Map_In_Play is
         when Fall     => Fall_Map.Clearings,
         when Winter   => Winter_Map.Clearings,
         when Lake     => Lake_Map.Clearings,
         when Mountain => Mountain_Map.Clearings);

   function Coords return Coordinates is
      (case Map_In_Play is
         when Fall     => Fall_Clearing_Coords,
         when Winter   => Winter_Clearing_Coords,
         when Lake     => Lake_Clearing_Coords,
         when Mountain => Mountain_Clearing_Coords);

   function Text_Map return Map_Text is
      (case Map_In_Play is
         when Fall     => Fall_Map_Base,
         when Winter   => Winter_Map_Base,
         when Lake     => Lake_Map_Base,
         when Mountain => Mountain_Map_Base);

   function Query_Suit (Prio : Priority) return Clearing_Suit is
      procedure Put_Clearing (Line, Col : Natural;
                              S : Clearing_Suit; Prio : Priority) is
         C : constant Color := (case S is
                                 when Fox    => Red,
                                 when Mouse  => Yellow,
                                 when Rabbit => B_Yellow);
         S_Char : constant Character := (case S is
                                          when Fox    => 'F',
                                          when Mouse  => 'M',
                                          when Rabbit => 'R');
      begin
         Cursor_Line_Move (Line);
         Cursor_Column_Set (Col);
         Set_Style (C);
         Put ("@---@");
         Cursor_Line_Move (1);
         Cursor_Column_Set (Col);

         Put ("|");
         Reset_Style;
         Put (" _ ");
         Set_Style (C);
         Put ("|");
         Cursor_Line_Move (1);
         Cursor_Column_Set (Col);

         Put (S_Char & "--");
         if Prio < 10 then
            Put ("-");
         end if;
         Set_Style (B_White);
         Int_IO.Put (Prio, Width => 0);
         Reset_Style;
         Cursor_Line_Move (0 - Line - 2);
         Cursor_Column_Set (Col);
      end Put_Clearing;

      B_Col : constant := (Root.IO.WIDTH - Map_Width) / 2 + 2;
   begin
      Erase_Screen;
      Cursor_Home;
      Put_Line_Centered (Map_In_Play'Image);
      for L of Text_Map loop
         Put_Line (To_String ((B_Col - 1) * ' ') & L);
      end loop;
      Cursor_Line_Move (0 - Text_Map'Length);
      Cursor_Column_Move (B_Col);

      for I in Priority'Range loop
         if Set_Clearings (I) /= Bird then
            Put_Clearing (Coords (I).x, B_Col + Coords (I).y,
                          Set_Clearings (I), I);
         end if;
      end loop;
      Cursor_Line_Move (Text_Map'Length);
      New_Line;
      Separator;
      New_Line;

      Put_Line ("What is the suit of clearing" & Prio'Image & "?");
      Set_Clearings (Prio) := Get_Clearing_Suit_Opt;
      return Set_Clearings (Prio);
   end Query_Suit;

   function Winter_Map return Map is
   begin
      if Winter_Map_Set then
         return Winter_Map_Actual;
      end if;

      for C of Set_Clearings loop
         C := Bird;
      end loop;

      Map_In_Play := Winter;

      Winter_Map_Actual :=
         (Winter,
         ((Query_Suit (1),  1, False, (5, 10, 11, 0, 0, 0)),
          (Query_Suit (2),  1, False, (6, 7, 12, 0, 0, 0)),
          (Query_Suit (3),  2, False, (7, 8, 12, 0, 0, 0)),
          (Query_Suit (4),  2, False, (9, 10, 11, 0, 0, 0)),
          (Query_Suit (5),  2, False, (1, 6, 0, 0, 0, 0)),
          (Query_Suit (6),  2, False, (5, 2, 0, 0, 0, 0)),
          (Query_Suit (7),  1, False, (2, 3, 0, 0, 0, 0)),
          (Query_Suit (8),  1, True,  (3, 9, 12, 0, 0, 0)),
          (Query_Suit (9),  1, True,  (4, 8, 11, 0, 0, 0)),
          (Query_Suit (10), 1, False, (1, 4, 0, 0, 0, 0)),
          (Query_Suit (11), 2, True,  (1, 4, 9, 0, 0, 0)),
          (Query_Suit (12), 2, True,  (2, 3, 8, 0, 0, 0))));
      Winter_Map_Set := True;

      return Winter_Map_Actual;
   end Winter_Map;

   function Lake_Map return Map is
   begin
      if Lake_Map_Set then
         return Lake_Map_Actual;
      end if;

      for C of Set_Clearings loop
         C := Bird;
      end loop;

      Map_In_Play := Lake;

      Lake_Map_Actual :=
         (Lake,
         ((Query_Suit (1),  2, False, (5, 9, 0, 0, 0, 0)),
          (Query_Suit (2),  1, False, (7, 8, 10, 0, 0, 0)),
          (Query_Suit (3),  1, False, (8, 9, 12, 0, 0, 0)),
          (Query_Suit (4),  1, False, (5, 6, 0, 0, 0, 0)),
          (Query_Suit (5),  2, True,  (1, 4, 11, 0, 0, 0)),
          (Query_Suit (6),  2, False, (4, 7, 11, 0, 0, 0)),
          (Query_Suit (7),  1, False, (2, 6, 10, 11, 0, 0)),
          (Query_Suit (8),  1, False, (2, 3, 10, 0, 0, 0)),
          (Query_Suit (9),  1, False, (1, 3, 12, 0, 0, 0)),
          (Query_Suit (10), 2, True,  (2, 7, 8, 0, 0, 0)),
          (Query_Suit (11), 2, True,  (5, 6, 7, 0, 0, 0)),
          (Query_Suit (12), 2, True,  (3, 9, 0, 0, 0, 0))));
      Lake_Map_Set := True;

      return Lake_Map_Actual;
   end Lake_Map;

   function Mountain_Map return Map is
   begin
      if Mountain_Map_Set then
         return Mountain_Map_Actual;
      end if;

      for C of Set_Clearings loop
         C := Bird;
      end loop;

      Map_In_Play := Mountain;

      Mountain_Map_Actual :=
         (Mountain,
         ((Query_Suit (1),  2, False, (8, 9, 0, 0, 0, 0)),
          (Query_Suit (2),  2, False, (5, 6, 11, 0, 0, 0)),
          (Query_Suit (3),  2, False, (6, 7, 11, 0, 0, 0)),
          (Query_Suit (4),  2, False, (8, 12, 0, 0, 0, 0)),
          (Query_Suit (5),  1, False, (2, 9, 10, 11, 0, 0)),
          (Query_Suit (6),  1, False, (2, 3, 11, 0, 0, 0)),
          (Query_Suit (7),  1, False, (3, 12, 0, 0, 0, 0)),
          (Query_Suit (8),  1, False, (1, 4, 9, 0, 0, 0)),
          (Query_Suit (9),  2, True,  (1, 5, 8, 10, 12, 0)),
          (Query_Suit (10), 1, True,  (5, 9, 11, 12, 0, 0)),
          (Query_Suit (11), 2, True,  (2, 3, 5, 6, 10, 12)),
          (Query_Suit (12), 2, True,  (4, 7, 9, 10, 11, 0))));
      Mountain_Map_Set := True;

      return Mountain_Map_Actual;
   end Mountain_Map;

   ------------------
   -- Map Printing --
   ------------------

   -- Line, Col : Relative Positioning --
   procedure Clearing_Box (Line : Natural; Col, Units, Buildings : Natural;
                           Rule : Boolean; Clear : Clearing; Pri : Priority) is
      C : constant Color := (case Clear.C_Suit is
                              when Fox    => Red,
                              when Mouse  => Yellow,
                              when Rabbit => B_Yellow);
      S : constant Character := (case Clear.C_Suit is
                                    when Fox    => 'F',
                                    when Mouse  => 'M',
                                    when Rabbit => 'R');
   begin
      Cursor_Line_Move (Line);
      Cursor_Column_Set (Col);
      if Buildings > 0 then
         Int_IO.Put (Buildings, Width => 0);
      else
         Put ("@");
      end if;
      Set_Style (C);
      Put ("---");
      if Rule then
         Reset_Style;
         Cursor_Column_Move (-2);
         Put ("^");
         Set_Style (C);
         Cursor_Column_Move (-3);
      else
         Cursor_Column_Move (-4);
      end if;
      Cursor_Line_Move (1);

      Put ("| ");
      Reset_Style;
      if Units >= 10 then
         Int_IO.Put (Units, Width => 0);
      else
         Int_IO.Put (Units, Width => 0);
         Put (" ");
      end if;
      Set_Style (C);
      Put ("|");
      Cursor_Line_Move (1);
      Cursor_Column_Move (-5);

      Put (S & "--");
      if Pri < 10 then
         Put ("-");
      end if;
      Set_Style (B_White);
      Int_IO.Put (Pri, Width => 0);
      Reset_Style;
      Cursor_Line_Move (0 - Line - 2);
      Cursor_Column_Set (Col);
   end Clearing_Box;

   procedure Put_Map (Units     : Warrior_Arr;
                      Buildings : Building_Arr;
                      Rule      : Rule_Arr) is
      B_Col    : constant := (Root.IO.WIDTH - Map_Width) / 2 + 2;
   begin
      Put_Line_Centered (Map_In_Play'Image);

      -- Print base map and return to TR corner --
      for L of Text_Map loop
         Put_Line (To_String ((B_Col - 1) * ' ') & L);
      end loop;
      Cursor_Line_Move (0 - Text_Map'Length);
      Cursor_Column_Move (B_Col);

      -- Print map details and numbers --
      for I in Priority'Range loop
         Clearing_Box (Coords (I).x, B_Col + Coords (I).y,
                       Units (I), Buildings (I), Rule (I),
                       Clearings (I), I);
      end loop;
      Cursor_Line_Move (Text_Map'Length);
   end Put_Map;

end Root.Maps;

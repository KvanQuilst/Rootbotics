with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Root.IO; use Root.IO;

package body Root.Maps is
   package Int_IO is new Integer_IO (Integer);

   procedure Query_Suits (S : out Clearing_Suit;
                          O : Clearing_Order) is
   begin
      New_Line;
      Put_Line ("For winter map, the clearing suits will be asked for in" &
                "order of left-to-right, top-to-bottom.");
      New_Line;

      for I in Priority'Range loop
         Put_Line ("What is the suit of clearing" &
                   O (I)'Image & ":");
         S (I) := Get_Suit_Opts;
         New_Line;
      end loop;
   end Query_Suits;

   function Winter_Map return Map is
      -- Order of map priority left to right, top to bottom --
      O : constant Clearing_Order :=
         (1, 5, 6, 2, 10, 11, 12, 7, 4, 9, 8, 3);
      S : Clearing_Suit;
   begin
      if Winter_Map_Set then
         return Winter_Map_Actual;
      end if;

      Query_Suits (S, O);

      Winter_Map_Actual :=
         (Winter,
         ((S (1),  1, False, (5, 10, 11, 0, 0, 0)),
          (S (2),  1, False, (6, 7, 12, 0, 0, 0)),
          (S (3),  2, False, (7, 8, 12, 0, 0, 0)),
          (S (4),  2, False, (9, 10, 11, 0, 0, 0)),
          (S (5),  2, False, (1, 6, 0, 0, 0, 0)),
          (S (6),  2, False, (5, 2, 0, 0, 0, 0)),
          (S (7),  1, False, (2, 3, 0, 0, 0, 0)),
          (S (8),  1, True,  (3, 9, 12, 0, 0, 0)),
          (S (9),  1, True,  (4, 8, 11, 0, 0, 0)),
          (S (10), 1, False, (1, 4, 0, 0, 0, 0)),
          (S (11), 2, True,  (1, 4, 9, 0, 0, 0)),
          (S (12), 2, True,  (2, 3, 8, 0, 0, 0))));
      Winter_Map_Set := True;

      return Winter_Map_Actual;
   end Winter_Map;

   function Lake_Map return Map is
      -- Order of map priority left to right, top to bottom --
      O : constant Clearing_Order :=
         (2, 7, 6, 4, 8, 10, 11, 5, 3, 12, 9, 1);
      S : Clearing_Suit;
   begin
      if Lake_Map_Set then
         return Lake_Map_Actual;
      end if;

      Query_Suits (S, O);

      Lake_Map_Actual :=
         (Lake,
         ((S (1),  2, False, (5, 9, 0, 0, 0, 0)),
          (S (2),  1, False, (7, 8, 10, 0, 0, 0)),
          (S (3),  1, False, (8, 9, 12, 0, 0, 0)),
          (S (4),  1, False, (5, 6, 0, 0, 0, 0)),
          (S (5),  2, True,  (1, 4, 11, 0, 0, 0)),
          (S (6),  2, False, (4, 7, 11, 0, 0, 0)),
          (S (7),  1, False, (2, 6, 10, 11, 0, 0)),
          (S (8),  1, False, (2, 3, 10, 0, 0, 0)),
          (S (9),  1, False, (1, 3, 12, 0, 0, 0)),
          (S (10), 2, True,  (2, 7, 8, 0, 0, 0)),
          (S (11), 2, True,  (5, 6, 7, 0, 0, 0)),
          (S (12), 2, True,  (3, 9, 0, 0, 0, 0))));
      Lake_Map_Set := True;

      return Lake_Map_Actual;
   end Lake_Map;

   function Mountain_Map return Map is
      -- Order of map priority left to right, top to bottom --
      O : constant Clearing_Order :=
         (1, 5, 2, 8, 9, 10, 12, 11, 6, 4, 7, 3);
      S : Clearing_Suit;
   begin
      if Mountain_Map_Set then
         return Mountain_Map_Actual;
      end if;

      Query_Suits (S, O);

      Mountain_Map_Actual :=
         (Mountain,
         ((S (1),  2, False, (8, 9, 0, 0, 0, 0)),
          (S (2),  2, False, (5, 6, 11, 0, 0, 0)),
          (S (3),  2, False, (6, 7, 11, 0, 0, 0)),
          (S (4),  2, False, (8, 12, 0, 0, 0, 0)),
          (S (5),  1, False, (2, 9, 10, 11, 0, 0)),
          (S (6),  1, False, (2, 3, 11, 0, 0, 0)),
          (S (7),  1, False, (3, 12, 0, 0, 0, 0)),
          (S (8),  1, False, (1, 4, 9, 0, 0, 0)),
          (S (9),  2, True,  (1, 5, 8, 10, 12, 0)),
          (S (10), 1, True,  (5, 9, 11, 12, 0, 0)),
          (S (11), 2, True,  (2, 3, 5, 6, 10, 12)),
          (S (12), 2, True,  (4, 7, 9, 10, 11, 0))));
      Mountain_Map_Set := True;

      return Mountain_Map_Actual;
   end Mountain_Map;

   ------------------
   -- Map Printing --
   ------------------

   procedure Clearing_Box (Line : Positive; Col, Units : Natural;
                           Clear : Clearing; Pri : Priority) is
      C : constant Color := (case Clear.C_Suit is
                             when Fox => Red,
                             when Mouse => Yellow,
                             when Rabbit => B_Yellow,
                             when Bird => B_Blue);
      S : constant Character := (case Clear.C_Suit is
                                 when Fox => 'F',
                                 when Mouse => 'M',
                                 when Rabbit => 'R',
                                 when Bird => 'B');
   begin
      Cursor_Set (Line, Col);
      Set_Style (C);
      Put ("@---@");
      Cursor_Set (Line + 1, Col);
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
      Cursor_Set (Line + 2, Col);
      Put (S & "--");
      if Pri < 10 then
         Put ("-");
      end if;
      Set_Style (B_Black);
      Int_IO.Put (Pri, Width => 0);
      Reset_Style;
   end Clearing_Box;

   procedure Put_Map_Fall (Units : Meeple_Arr) is
      B_Line : constant Positive := Positive (Line) - 1;
      B_Col  : constant Positive := (Root.IO.WIDTH - Fall_Map_Width) / 2 + 2;
   begin
      Put_Line_Centered ("FALL");
      for L of Fall_Map_Base loop
         Put (To_String ((B_Col - 1) * ' '));
         Put_Line (L);
      end loop;

      for I in Priority'Range loop
         Clearing_Box (B_Line + Fall_Clearing_Coords (I, 1), 
                       B_Col  + Fall_Clearing_Coords (I, 2),
                       Units (I), Fall_Map.Clearings (I), I);
      end loop;
      Cursor_Set (B_Line + Fall_Map_Height, 0);
   end Put_Map_Fall;

   procedure Put_Map (Map : Map_Name; Units : Meeple_Arr) is
   begin
      if Map = Fall then
         Put_Map_Fall (Units);
      end if;
   end Put_Map;

end Root.Maps;

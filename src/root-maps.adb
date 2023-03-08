with Ada.Text_IO; use Ada.Text_IO;

with Root.IO; use Root.IO;

package body Root.Maps is

   procedure Query_Suits (S : out Clearing_Suit;
                          O : Clearing_Order) is
      Opt : Character;
   begin
      New_Line;
      Put_Line ("For winter map, the clearing suits will be asked for in" &
                "order of left-to-right, top-to-bottom.");
      New_Line;

      for I in Priority'Range loop
         Put_Line ("What is the suit of clearing" &
                   O (I)'Image & ":");
         Opt := Get_Suit_Opts;
         New_Line;

         S (I) := Suit'Val (Character'Pos (Opt) - 97);
      end loop;
   end Query_Suits;

   function Winter_Map return Map is
      -- Order of map priority left to right, top to bottom --
      O : constant Clearing_Order :=
         (1, 5, 6, 2, 10, 11, 12, 7, 4, 9, 8, 3);
      S : Clearing_Suit;

      M : Map;
   begin
      Query_Suits (S, O);

      M := (Winter,
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

      return M;
   end Winter_Map;

   function Lake_Map return Map is
      -- Order of map priority left to right, top to bottom --
      O : constant Clearing_Order :=
         (2, 7, 6, 4, 8, 10, 11, 5, 3, 12, 9, 1);
      S : Clearing_Suit;

      M : Map;
   begin
      Query_Suits (S, O);

      M := (Lake,
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

      return M;
   end Lake_Map;

   function Mountain_Map return Map is
      -- Order of map priority left to right, top to bottom --
      O : constant Clearing_Order :=
         (1, 5, 2, 8, 9, 10, 12, 11, 6, 4, 7, 3);
      S : Clearing_Suit;

      M : Map;
   begin
      Query_Suits (S, O);

      M := (Mountain,
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

      return M;
   end Mountain_Map;

end Root.Maps;

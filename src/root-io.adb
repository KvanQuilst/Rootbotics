with Ada.Text_IO; use Ada.Text_IO;

with Root.Maps; use Root.Maps;

package body Root.IO is
   package Int_IO is
      new Integer_IO (Integer); use Int_IO;

  ---------------------------- -- Get Checked User Input --
  ----------------------------

   procedure Put_Options (Options : String_Arr) is
   begin
      pragma Assert (Options'Length /= 0);

      -- Print options a - (a + Num_Opts) --
      Separator;
      for I in Options'Range loop
         Put_Line (" " & Character'Val (96 + I) & ". " &
           To_String (Options (I)));
      end loop;
      Separator;
   end Put_Options;

   function Get_Option (Options  : String_Arr) return Character is
      C    : Character;

      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      Put_Options (Options);

      -- Check character is between 'a' and '(a + Num_Opts)' --
      loop
         Put ("Option: ");
         Get (C);
         Line := To_Unbounded_String (Get_Line);
         exit when Character'Pos (C) - 96 > 0 and then
           Character'Pos (C) - 96 <= Options'Length;
         Put_Line ("Invalid option!");
      end loop;

      return C;
   end Get_Option;

   function Get_Options (Options  : String_Arr) return Char_Arr is
      Opts    : Char_Arr (1 .. Options'Length);
      Line    : Unbounded_String;
      Count   : Integer := 0;
      Invalid : Boolean;
   begin
      Put_Options (Options);

      Put_Line ("Enter the options applicable. Press enter for 'none'...");

      loop
         Invalid := False;
         Count   := 0;

         Put ("Options: ");

         Line := To_Unbounded_String (Get_Line);

         -- Get number of options / determine invalid --
         for I in 1 .. Length (Line) loop
            if Element (Line, I) >= 'a' and then
              Element (Line, I) <= Character'Val (96 + Options'Length)
            then
               Count := Count + 1;
               Opts (Count) := Element (Line, I);
            elsif Element (Line, I) /= ' ' then
               Invalid := True;
            end if;
         end loop;

         Invalid := (if Count > Options'Length then True else Invalid);

         for I in 1 .. Count - 1 loop
            for J in I + 1 .. Count loop
               Invalid := (if Opts (I) = Opts (J) then True else Invalid);
            end loop;
         end loop;

         exit when Invalid = False;
         Put_Line ("Invalid response!");
      end loop;

      return Opts (1 .. Count);
   end Get_Options;

   function Get_Integer (Low, High : Integer) return Integer is
      C    : Character;
      EOL  : Boolean;
      Val  : Integer := -1;

      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      loop
         Put ("Response: ");
         Look_Ahead (C, EOL);
         if C >= '0' and then C <= '9' then
            Get (Val);
         end if;

         exit when Val >= Low and then Val <= High;

         Put_Line ("Invalid input!");
         Line := To_Unbounded_String (Get_Line);
      end loop;
      Line := To_Unbounded_String (Get_Line);
      return Val;
   end Get_Integer;

   function Get_Integers (Low, High : Integer) return Int_Arr is
      Ints : Int_Arr (1 .. 1);
   begin
      return Ints;
   end Get_Integers;

   function Get_Yes_No return Boolean is
      C    : Character;

      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      loop
         Put ("Response (y/n): ");
         Get (C);
         Line := To_Unbounded_String (Get_Line);

         exit when C = 'y' or else C = 'Y' or else
                   C = 'n' or else C = 'N';

         Put_Line ("Invalid input!");
      end loop;

      return C = 'y' or else C = 'Y';
   end Get_Yes_No;

   -----------------
   -- Common Gets --
   -----------------

   function Get_Suit_Opts return Suit is
      S : constant String_Arr := (
         To_Unbounded_String (Fox),
         To_Unbounded_String (Mouse),
         To_Unbounded_String (Rabbit),
         To_Unbounded_String (Bird)
         );
   begin
      return Suit'Val (Character'Pos (Get_Option (S)) - 97);
   end Get_Suit_Opts;

   ----------------
   -- Formatting --
   ----------------

   procedure Put_Line_Centered (S : String) is
      Start, Length : Integer;
      Escape : Boolean := False;
   begin
      -- Account for terminal escape codes --
      Length := S'Length;
      for I in S'Range loop
         if Escape then
            Length := Length - 1;
            Escape := S (I) /= 'm';
         else
            Escape := S (I) = ESC;
         end if;
      end loop;

      Start := (WIDTH / 2) - (Length / 2);
      for I in 1 .. Start loop
         Put (" ");
      end loop;
      Put_Line (S);
   end Put_Line_Centered;

   procedure Set_Style (FG : Color;
                        S  : Style := None) is
   begin
      Put (ESC & "[");

      -- Style --
      if S /= None then
         Put (Style'Enum_Rep (S), 0);
         Put (";");
      end if;

      -- FG --
      Put (Color'Enum_Rep (FG), 0);
      Put ("m");
   end Set_Style;

   function String_Style (Str : String;
                          FG  : Color;
                          S   : Style := None) return String is
      Out_Str : Unbounded_String;
      Val     : String (1 .. 2);
   begin
      Out_Str := To_Unbounded_String (ESC & "[");

      -- Style --
      if S /= None then
         if Style'Enum_Rep (S) < 10 then
            declare
               Val : String (1 .. 1);
            begin
               Put (Val, Style'Enum_Rep (S));
               Out_Str := Out_Str & Val & ";";
            end;
         else
            Put (Val, Style'Enum_Rep (S));
            Out_Str := Out_Str & Val & ";";
         end if;
      end if;

      -- FG --
      Put (Val, Color'Enum_Rep (FG));
      Out_Str := Out_Str & Val & "m" & Str & ESC & "[0m";

      return To_String (Out_Str);
   end String_Style;

   procedure Reset_Style is
   begin
      Put (ESC & "[0m");
   end Reset_Style;

   ---------------------
   -- Cursor Controls --
   ---------------------
   procedure Cursor_Home is
   begin
      Put (ESC & "[H");
   end Cursor_Home;

   procedure Cursor_Set (Line : Positive; Column : Natural) is
   begin
      Put (ESC & "[");
      Put (Line, Width => 0);
      Put (";");
      Put (Column, Width => 0);
      Put ("H");
   end Cursor_Set;

   procedure Cursor_Line_Move (Num_Lines : Integer) is
      Val : constant Natural := (if Num_Lines < 0
                                 then Natural (0 - Num_Lines)
                                 else Natural (Num_Lines));
   begin
      Put (ESC & "[");
      Put (Val, Width => 0);
      if Num_Lines < 0 then
         Put ("A");
      elsif Num_Lines > 0 then
         Put ("B");
      end if;
   end Cursor_Line_Move;

   procedure Cursor_Column_Move (Num_Columns : Integer) is
      Val : constant Natural := (if Num_Columns < 0
                                 then Natural (0 - Num_Columns)
                                 else Natural (Num_Columns));
   begin
      Put (ESC & "[");
      Put (Val, Width => 0);
      if Num_Columns < 0 then
         Put ("D");
      elsif Num_Columns > 0 then
         Put ("C");
      end if;
   end Cursor_Column_Move;

   procedure Cursor_Column_Set (Column : Natural) is
   begin
      Put (ESC & "[");
      Put (Column, Width => 0);
      Put ("G");
   end Cursor_Column_Set;

   ---------------------
   -- Erase Functions --
   ---------------------

   procedure Erase_Screen is
   begin
      Put (ESC & "[2J");
   end Erase_Screen;

   -------------------
   -- Common Prints --
   -------------------

   procedure Continue is
      Line : Unbounded_String;
      pragma Unreferenced (Line);
   begin
      New_Line;
      Put ("Press enter to continue...");
      Line := To_Unbounded_String (Get_Line);
      New_Line;
   end Continue;

   procedure Separator is
   begin
      Put_Line (To_String (WIDTH * "-"));
   end Separator;

   procedure Put_Birdsong is
   begin
      New_Line;
      Put_Line_Centered (To_String (16 * "-"));
      Set_Style (FG => Yellow, S => Italic);
      Put_Line_Centered ("Birdsong");
      Reset_Style;
      Put_Line_Centered (To_String (16 * "-"));
      New_Line;
   end Put_Birdsong;

   procedure Put_Daylight is
   begin
      New_Line;
      Put_Line_Centered (To_String (16 * "-"));
      Set_Style (FG => B_Cyan, S => Italic);
      Put_Line_Centered ("Daylight");
      Reset_Style;
      Put_Line_Centered (To_String (16 * "-"));
      New_Line;
   end Put_Daylight;

   procedure Put_Evening is
   begin
      New_Line;
      Put_Line_Centered (To_String (15 * "-"));
      Set_Style (FG => B_Blue, S => Italic);
      Put_Line_Centered ("Evening");
      Reset_Style;
      Put_Line_Centered (To_String (15 * "-"));
      New_Line;
   end Put_Evening;

   procedure Put_Prompt (Put_Logo : access procedure;
                         Put_State : access procedure;
                         Units : Warrior_Arr;
                         Current_Order : Suit) is
   begin
      -- Logo --
      Erase_Screen;
      Cursor_Home;
      Put_Logo.all;

      -- Map --
      Put_Map (Units);
      New_Line;
      Separator;

      -- State --
      Put_State.all;
      New_Line;
      Put ("    Current Order: ");
      Put_Line ((case Current_Order is
                  when Root.Fox    => Root.IO.Fox,
                  when Root.Rabbit => Root.IO.Rabbit,
                  when Root.Mouse  => Root.IO.Mouse,
                  when Root.Bird   => Root.IO.Bird));
      Separator;

   end Put_Prompt;

end Root.IO;

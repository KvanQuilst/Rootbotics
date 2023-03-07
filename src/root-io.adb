package body Root.IO is
  package Int_IO is new Integer_IO (Integer); use Int_IO;

  ----------------------------
  -- Get Checked User Input --
  ----------------------------
 
  procedure Put_Options (Options : String_Arr) is
  begin
    pragma Assert (Options'Length /= 0);

    -- Print options 'a' - '(a + Num_Opts)' --
    Separator;
    for I in Options'Range loop
      Put_Line (" " & Character'Val (96 + I) & ". " &
                To_String (Options (I)));
    end loop;
    Separator;
  end Put_Options;

  function Get_Option (Options  : String_Arr) return Character is
    Line : Unbounded_String;
    C    : Character;
  begin
    Put_Options (Options);

    -- Check character is between 'a' and '(a + Num_Opts)'
    loop
      Put ("Option: ");
      Get (C);
      Line := To_Unbounded_String (Get_Line);
      exit when Character'Pos (C) - 96 > 0 and
                Character'Pos (C) - 96 <= Options'Length;
      Put_Line ("Invalid option!");
    end loop;

    return C;
  end Get_Option;

  function Get_Options (Options  : String_Arr) return Char_Arr is
    Opts    : Char_Arr (1..Options'Length);
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
      for I in 1..Length (Line) loop
        if Element (Line, I) >= 'a' and 
           Element (Line, I) <= Character'Val (96 + Options'Length) then
          Count := Count + 1;
          Opts (Count) := Element (Line, I);
        elsif Element (Line, I) /= ' ' then
          Invalid := True;
        end if;
      end loop;

      Invalid := (if Count > Options'Length then True else Invalid);  

      for I in 1..Count-1 loop
        for J in I+1..Count loop
          Invalid := (if Opts (I) = Opts (J) then True else Invalid);
        end loop;
      end loop;

      exit when Invalid = False;
      Put_Line ("Invalid response!");
    end loop;
    return Opts (1..Count);
  end Get_Options;

  function Get_Integer (Low, High : Integer) return Integer is
    Line : Unbounded_String;
    C    : Character;
    EOL  : Boolean;
    Val  : Integer;
  begin
    loop
      Put ("Response: ");
      Look_Ahead (C, EOL);
      exit when C >= '0' and C <= '9';
      Put_Line ("Invalid input!");
      Line := To_Unbounded_String (Get_Line);
    end loop;

    Get (Val);
    Line := To_Unbounded_String (Get_Line);
    return Val;
  end Get_Integer;

  function Get_Yes_No return Boolean is
    Line : Unbounded_String;
    C    : Character;
  begin
    loop
      Put ("Response: ");
      Get (C);
      Line := To_Unbounded_String (Get_Line);

      exit when C = 'y' or C = 'Y' or
                C = 'n' or C = 'N';
      Put_Line ("Invalid input!");
    end loop;

    return C = 'y' or C = 'Y';
  end Get_Yes_No;


  -----------------
  -- Common Gets --
  -----------------

  function Get_Suit_Opts return Character is
    S : String_Arr := (
      To_Unbounded_String (Fox),
      To_Unbounded_String (Mouse),
      To_Unbounded_String (Rabbit),
      To_Unbounded_String (Bird)
      );
  begin
    return Get_Option (S);
  end Get_Suit_Opts;
  

  ----------------
  -- Formatting --
  ----------------

  procedure Put_Line_Centered (S : String) is
    Start : Integer;
  begin
    Start := (WIDTH / 2) - (S'Length / 2);
    for I in 1..Start loop
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
    Val     : String (1..2);
  begin
    Out_Str := To_Unbounded_String (ESC & "[");

    -- Style --
    if S /= None then
      if Style'Enum_Rep (S) < 10 then
        declare
          Val : String (1..1);
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


  -------------------
  -- Common Prints --
  -------------------

  procedure Continue is
    Line : Unbounded_String;
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

end Root.IO;

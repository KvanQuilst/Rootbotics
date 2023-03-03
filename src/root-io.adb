with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Root.IO is
  package Int_IO is new Integer_IO (Integer); use Int_IO;

  ----------------------------
  -- Get Checked User Input --
  ----------------------------
 
  -- Looks only at first character of line! --
  function Get_Option (Num_Opts : Integer) return Character is
    C : Character;
    Line : Unbounded_String;
  begin
    pragma Assert (Num_Opts > 0 and Num_Opts <= 26);

    Put ("Option: ");
    Get (C);
    Line := To_Unbounded_String (Get_Line);

    -- Check character is between 'a' and '(a + Num_Opts)'
    while Character'Pos (C) - 96 < 1 or Character'Pos (C) - 96 > Num_Opts loop
      Put_Line ("Invalid option!");
      Put ("Option: ");
      Get (C);
      Line := To_Unbounded_String (Get_Line);
    end loop;

    return C;
  end Get_Option;

  function Get_Option (Num_Opts : Integer;
                       Options  : String_Arr) return Character is
  begin
    pragma Assert (Num_Opts = Options'Length);

    -- Print options 'a' - '(a + Num_Opts)' --
    Separator;
    for I in Options'Range loop
      Put_Line (" " & Character'Val (96 + I) & ". " &
                To_String (Options (I)));
    end loop;
    Separator;

    return Get_Option (Num_Opts);
  end Get_Option;


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
    return Get_Option (4, S);
  end Get_Suit_Opts;
  

  ----------------
  -- Formatting --
  ----------------

  procedure Put_Line_Centered (S : String) is
    Start : Integer;
  begin
    Start := (WIDTH / 2) - (S'Length / 2) + 1;
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

  procedure Reset_Style is
  begin
    Put (ESC & "[0m");
  end Reset_Style;


  ----------------------------
  -- Common Colored Strings --
  ----------------------------
  
  function Fox return String is
  begin
    return ESC & "[91mFox" & ESC & "[0m";
  end Fox;

  function Mouse return String is
  begin
    return ESC & "[33mMouse" & ESC & "[0m";
  end Mouse;

  function Rabbit return String is
  begin
    return ESC & "[93mRabbit" & ESC & "[0m";
  end Rabbit;

  function Bird return String is
  begin
    return ESC & "[94mBird" & ESC & "[0m";
  end Bird;


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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Root.IO is
  package Int_IO is new Integer_IO (Integer); use Int_IO;


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

  procedure Put_Suit_Opts is
  begin
    Separator;
    
    Put (" a. ");
    Set_Style (B_Red);
    Put_Line ("Fox");
    Reset_Style;

    Put (" b. ");
    Set_Style (Yellow);
    Put_Line ("Mouse");
    Reset_Style;

    Put (" c. ");
    Set_Style (B_Yellow);
    Put_Line ("Rabbit");
    Reset_Style;

    Put (" d. ");
    Set_Style (B_Blue);
    Put_Line ("Bird");
    Reset_Style;

    Separator;
  end Put_Suit_Opts;

end Root.IO;

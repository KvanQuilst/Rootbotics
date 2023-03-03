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
                       BG : Color := None;
                       S  : Style := None) is
  begin
    Put (ESC & "[");
    
    -- Style --
    if S /= None then
      Put (Style'Enum_Rep (S), 0);
      Put (";");
    end if;

    -- BG --
    case BG is
      when Black     => Put ("40;");
      when Red       => Put ("41;");
      when Green     => Put ("42;");
      when Yellow    => Put ("43;");
      when Blue      => Put ("44;");
      when Magenta   => Put ("45;");
      when Cyan      => Put ("46;");
      when White     => Put ("47;");
      when B_Black   => Put ("100;");
      when B_Red     => Put ("101;");
      when B_Green   => Put ("102;");
      when B_Yellow  => Put ("103;");
      when B_Blue    => Put ("104;");
      when B_Magenta => Put ("105;");
      when B_Cyan    => Put ("106;");
      when B_White   => Put ("107;");
      when Default   => Put ("49;");
      when None      => null;
    end case;

    -- FG --
    case FG is
      when Black     => Put ("30m");
      when Red       => Put ("31m");
      when Green     => Put ("32m");
      when Yellow    => Put ("33m");
      when Blue      => Put ("34m");
      when Magenta   => Put ("35m");
      when Cyan      => Put ("36m");
      when White     => Put ("37m");
      when B_Black   => Put ("90m");
      when B_Red     => Put ("91m");
      when B_Green   => Put ("92m");
      when B_Yellow  => Put ("93m");
      when B_Blue    => Put ("94m");
      when B_Magenta => Put ("95m");
      when B_Cyan    => Put ("96m");
      when B_White   => Put ("97m");
      when Default   => Put ("39m");
      when None      => Put ("39m");
    end case;
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

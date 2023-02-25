with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Root is
  package Int_IO is new Integer_IO (Integer); use Int_IO;

  procedure Get_Option (C : out Character; Num_Opts : Integer) is
  begin
    Put ("Option: ");
    Get (C);

    while Character'Pos (C) - 96 < 1 or Character'Pos (C) - 96 > Num_Opts loop
      Put_Line ("Invalid option!");
      Put ("Option: ");
      Get (C);
    end loop;
  end Get_Option;

  function Get_YN return Boolean is
    Line : Unbounded_String;
    C : Character := 'a';
  begin
    Put ("Option: ");
    Get (C);
    while C /= 'y' or C /= 'n' loop
      Put_Line ("Invalid input!");
      Put ("Option: ");
      Get (C);
      Line := To_Unbounded_String (Get_Line);
    end loop;
    return C = 'y';
  end Get_YN;

  function Get_Input_Internal (I : out Integer; Low, High : Integer) return Boolean is
    Line : Unbounded_String;
    Val  : Integer range Low .. High;
  begin
    Get (Val);
    Line := To_Unbounded_String (Get_Line);
    I := Val;
    return true;
  exception
    when others =>
      return False;
  end Get_Input_Internal;

  procedure Get_Input (I : out Integer; Low, High : Integer) is
  begin
    while not Get_Input_Internal (I, Low, High) loop
      Put_Line ("Invalid input!");
    end loop;
  end Get_Input;

  function Get_List_Internal (PL : out Priority_List) return Boolean is
    Line : Unbounded_String; 
    Last : Integer := 0;
    Count : Integer := 1;
  begin
    for I in PL'Range loop
      PL (I) := 0;
    end loop;

    Line := To_Unbounded_String (Get_Line);

    while Last /= Length (Line) loop
      Get (Slice (Line, Last+1, Length(Line)), PL (Count), Last);
      Count := Count + 1;
    end loop;
    return True;
  exception
    when others =>
      return False;
  end Get_List_Internal;

  -- Get space serparated list of priority (clearings) --
  -- Checks for errors                     --
  function Get_List (Values : String) return Priority_List is
    PL : Priority_List;
  begin
    Put_Line ("Provide a space separated list of " & Values & ":");
    while not Get_List_Internal(PL) loop
      Put_Line ("Invalid input!");
      Put_Line ("Provide a space separated list of " & Values & ":");
    end loop;
    return PL;
  end Get_List;


  function Get_List_Internal (OL : out Option_List; Num_Opts : Positive) return Boolean is
    Line : Unbounded_String;
    Last : Integer := 0;
    Count : Integer := 1;
    Opt : Character range 'a' .. Character'Val (96 + Num_Opts);
  begin
    for I in OL'Range loop
      OL (I) := Character'Val (0);
    end loop;

    Line := To_Unbounded_String (Get_Line);

    for I in 1..Length (Line) loop
      if Element (Line, I) /= ' ' then
        Opt := Element (Line, I);
        OL (Count) := Opt;
        Count := Count + 1;
      end if;
    end loop;
    return True;
  exception
    when others =>
      return False;
  end Get_List_Internal;

  -- Get space separated list of options --
  -- Checks for error                    --
  procedure Get_List (OL : out Option_List; Num_Opts : Positive) is
  begin
    Put_Line ("Provide a space separated list of options that apply:");
    while not Get_List_Internal (OL, Num_Opts) loop
      Put_Line ("Invalid input!");
      Put_Line ("Provide a spae separated list of options that apply:");
    end loop;
  end Get_List;


  procedure Wait_Continue is
    Line : Unbounded_String;
  begin
    New_Line;
    Put ("Press enter to continue...");
    Line := To_Unbounded_String (Get_Line);
    New_Line;
  end Wait_Continue;

  procedure Separator is
  begin
    Put_Line ("-------------------------");
  end Separator;

  procedure Put_Line_Center (S : String) is
    Start : Integer;
  begin
    Start := (WIDTH / 2) - (S'Length / 2) + 1;
    for I in 1..Start loop
      Put (" ");
    end loop;
    Put_Line (S);
  end Put_Line_Center;

  procedure Put_Birdsong is
  begin
    New_Line; 
    Separator;
    Put_Line_Center ("Birdsong");
    Separator;
    New_Line;
  end Put_Birdsong;

  procedure Put_Daylight is
  begin
    New_Line; 
    Separator;
    Put_Line_Center ("Daylight");
    Separator;
    New_Line;
  end Put_Daylight;

  procedure Put_Evening is
  begin
    New_Line; 
    Separator;
    Put_Line_Center ("Evening");
    Separator;
    New_Line;
  end Put_Evening;
end Root;

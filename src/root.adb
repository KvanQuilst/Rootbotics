with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Root is
  package Int_IO is new Integer_IO (Integer); use Int_IO;

  procedure Get_Input (C : out Character; Num_Opts : Integer) is
  begin
    Put ("Option: ");
    Get (C);

    while Character'Pos (C) - 96 < 1 or Character'Pos (C) - 96 > Num_Opts loop
      Put_Line ("Invalid option!");
      Put ("Option: ");
      Get (C);
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


  function Get_List_Internal (OL : out Option_List;
                               Num_Opts : Positive) return Boolean is
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
  function Get_List (Num_Opts : Positive) return Option_List is
    OL : Option_List (1..Num_Opts);
  begin
    Put_Line ("Provide a space separated list of options that apply:");
    while not Get_List_Internal (OL, Num_Opts) loop
      Put_Line ("Invalid input!");
      Put_Line ("Provide a spae separated list of options that apply:");
    end loop;
    return OL;
  end Get_List;

end Root;

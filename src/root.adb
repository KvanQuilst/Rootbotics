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

  function Get_List_Internal (Values : String; IL : out Input_List) return Boolean is
    Line : Unbounded_String; 
    Last : Integer := 0;
    Count : Integer := 1;
  begin
    begin
      for I in IL'Range loop
        IL (I) := 0;
      end loop;

      Put_Line ("Provide a space separated list of " & Values & ":");
      Line := To_Unbounded_String (Get_Line);
      Put_Line (To_String(Line));

      while Last /= Length (Line) loop
        Put_Line ("Last:" & Last'Image & " Length:" & Length(Line)'Image);
        Get (Slice (Line, Last+1, Length(Line)), IL (Count), Last);
        Count := Count + 1;
      end loop;
      return True;
    exception
      when others =>
        return False;
    end;
  end Get_List_Internal;

  -- Get space serparated list of integers --
  -- Checks for errors                     --
  function Get_List (Values : String) return Input_List is
    IL : Input_List (1..12);
  begin
    while not Get_List_Internal(Values, IL) loop
      Put_Line ("Invalid input!");
    end loop;
    return IL;
  end Get_List;

end Root;

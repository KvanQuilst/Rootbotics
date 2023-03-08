with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Root is
   package Int_IO is new Integer_IO (Integer); use Int_IO;

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
         Get (Slice (Line, Last + 1, Length (Line)), PL (Count), Last);
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
      while not Get_List_Internal (PL) loop
         Put_Line ("Invalid input!");
         Put_Line ("Provide a space separated list of " & Values & ":");
      end loop;
      return PL;
   end Get_List;

end Root;

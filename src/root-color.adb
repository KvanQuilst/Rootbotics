with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body Root.Color is
  package Int_IO is new Integer_IO (Integer); use Int_IO;

  procedure Set_Color (FG : Color; BG : Color := None) is
  begin
    Put (ESC & "[38;5;");
    Put (Color'Enum_Rep (FG), 0);
    Put ('m');
    if BG /= None then
      Put (ESC & "[48;5;");
      Put (Color'Enum_Rep (BG), 0);
      Put ('m');
    end if;
  end Set_Color;

  procedure Set_Style (S : Style) is
  begin
    Put (ESC & "[");
    Put (Style'Enum_Rep (S), 0);
    Put ('m');
  end Set_Style;

  procedure Reset_Style is
  begin
    Put (ESC & "[0m");
  end Reset_Style;

end Root.Color;

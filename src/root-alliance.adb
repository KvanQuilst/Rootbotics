with Ada.Text_IO; use Ada.Text_IO;

package body Root.Alliance is

  procedure Put_Name (NewLine : Boolean := False) is
  begin
    Set_Style (Green);
    Put ("Automated Alliance");
    Reset_Style;
    if NewLine then
      New_Line;
    end if;
  end Put_Name;

  -------------------
  -- Faction Setup --
  -------------------

  function Setup (Diff : Difficulty) return Boolean is
  begin
    return True;
  end Setup;


  ---------------
  -- Take Turn --
  ---------------

  procedure Take_Turn (Order : Suit; M : Map) is
  begin

    -- Alliance State --
    Separator;
    New_Line;
    Set_Style (Green);
    Put_Line_Centered ("Automated Alliance");
    Reset_Style;
    -- F  M  R
    -- X  X  X
    -- Sympathetic Clearings: X
    New_Line;
    Separator;

    -- Birdsong --
    Put_Birdsong;

    Continue;

    -- Daylight --
    Put_Daylight;

    Continue;

    -- Evening --
    Put_Evening; 
    
    Continue;

  end Take_Turn;

end Root.Alliance;

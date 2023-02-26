with Ada.Text_IO; use Ada.Text_IO;

package body Root.Alliance is

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

  procedure Take_Turn (Order : Suit; M : Map_T) is
  begin

    -- Alliance State --
    Separator;
    New_Line;
    Put_Line_Center ("Automated Alliance");
    -- F  M  R
    -- X  X  X
    -- Sympathetic Clearings: X
    New_Line;
    Separator;

    -- Birdsong --
    Put_Birdsong;

    Wait_Continue;

    -- Daylight --
    Put_Daylight;

    Wait_Continue;

    -- Evening --
    Put_Evening; 
    
    Wait_Continue;

  end Take_Turn;

end Root.Alliance;

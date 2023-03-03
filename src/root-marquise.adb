with Ada.Text_IO; use Ada.Text_IO;

with Root.IO; use Root.IO;

package body Root.Marquise is

  procedure Put_Name (NewLine : Boolean := False) is
  begin
    Set_Style (Yellow);
    Put ("Mechanical Marquise 2.0");
    Reset_Style;
    if NewLine then
      New_Line;
    end if;
  end Put_Name;

  -------------------
  -- Faction Setup --
  -------------------

  procedure Setup (M : Map) is
    Corner : Integer range 1..4;
  begin
    Put ("Which corner clearing will the "); Put_Name; Put (" start in: ");
    Get_Input (Corner, 1, 4);

    -- Place starting pieces --
    for I in Meeples'Range loop
      Meeples (I) := 1;
    end loop;
    Meeples (Corner) := Meeples (Corner) + 1;

    if M.Name = Lake then
      case Corner is
        when 1 => Meeples (2) := 0;
        when 2 => Meeples (1) := 0;
        when 3 => Meeples (4) := 0;
        when 4 => Meeples (3) := 0;
      end case;
    else
      case Corner is
        when 1 => Meeples (3) := 0;
        when 2 => Meeples (4) := 0;
        when 3 => Meeples (1) := 0;
        when 4 => Meeples (2) := 0;
      end case;
    end if;

  end Setup;

  ---------------
  -- Take Turn --
  ---------------
  procedure Warriors_Lost (M : Map);
  procedure Battle  (S : Suit; M : Map);
  procedure Recruit (S : Suit; M : Map);
  function  Build   (S : Suit; M : Map) return Boolean;
  procedure Move    (S : Suit; M : Map);

  procedure Take_Turn (Order : Suit; M : Map) is
    Expand : Boolean := True;
  begin

    -- Mechanical Marquise 2.0 State --
    Separator;
    New_Line;
    Set_Style (Yellow);
    Put_Line_Centered ("Mechanical Marquise 2.0");
    Reset_Style;
    New_Line;
    Put_Line ("    Meeple Supply:" & Meeple_Supply'Image);
    Put_Line ("   Sawmill Supply:" & Sawmill_Supply'Image);
    Put_Line ("  Workshop Supply:" & Workshop_Supply'Image);
    Put_Line (" Recruiter Supply:" & Recruiter_Supply'Image);
    New_Line;
    Put_Line (" Current Order: " & Order'Image);
    New_Line;
    Separator;

    Warriors_Lost (M);

    -- Have the marquise lost? --
    if Meeple_Supply = MEEPLE_SUPPLY then
      New_Line;
      Put ("The "); Put_Name; Put_Line (" cannot do anything!");
      return;
    end if;

    -- Birdsong --
    Put_Birdsong;
  
    Put_Line ("Craft order card for (+ 1) if it has an available item.");

    Continue;

    -- Daylight --
    Put_Daylight;

    while Expand loop

      -- Battle --
      Put_Line ("--  Battle");
      Battle (Order, M);

      Continue;

      -- Recruit --
      Put_Line ("--  Recruit");
      Recruit (Order, M);

      Continue;

      -- Build --
      Put_Line ("--  Build");
      Expand := not Build (Order, M);

      -- Move --
      Put_Line ("--  Move");
      Put_Line ("Unimplemented!");

    end loop;

    -- Evening --
    Put_Evening;

    Put ("Score  (+");
    declare
      P : Integer;
    begin
      case Order is
        when Fox =>
          P := (if Sawmill_Supply /= 6 then 6 - Sawmill_Supply - 1 else 0);
        when Mouse =>
          P := (if Workshop_Supply /= 6 then 6 - Sawmill_Supply - 1 else 0);
        when Rabbit =>
          P := (if Recruiter_Supply /= 6 then 6 - Sawmill_Supply - 1 else 0);
        when Bird =>
          -- Find building track with least remaining buildings --
          P := (if Sawmill_Supply < Workshop_Supply 
            then Sawmill_Supply else Workshop_Supply);
          P := (if P < Recruiter_Supply then P else Recruiter_Supply);

          -- Score for that track --
          P := (if P /= 6 then 6 - P - 1 else 0);
      end case;
    end;
    Put (") points for the "); Put_Name (True);

  end Take_Turn;

  procedure Warriors_Lost (M : Map) is
  begin
    -- Determine lost warriors --
    for I in Priority'Range loop

      -- Warriors --
      if Meeples (I) > 0 then
        Put ("Do the "); Put_Name; Put_Line (" still have" &
                  Meeples (I)'Image & " warrior(s) in clearing" & I'Image &
                  "? (y/n)");
        if Get_YN then
          Put ("How many warriors remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, MEEPLE_MAX);
            Meeples (I) := Val;
          end;
        end if;
      end if;

      -- Sawmills --
      if Sawmill_Supply > 0 then
        Put_Line ("Do the "); Put_Name; Put_Line (" still have" &
                  Sawmill (I)'Image & " sawmill(s) in clearing" &
                  I'Image & "? (y/n)");
        if Get_YN then
          Put ("How many workshops remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, M.Clearings (I).Buildings);
            Sawmill (I) := Val;
          end;
        end if;
      end if;

      -- Workshop --
      if Sawmill_Supply > 0 then
        Put ("Do the "); Put_Name; Put_Line (" still have" &
                  Workshops (I)'Image & " workshop(s) in clearing" &
                  I'Image & "? (y/n)");
        if Get_YN then
          Put ("How many workshops remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, M.Clearings (I).Buildings);
            Workshops (I) := Val;
          end;
        end if;
      end if;

      -- Recruiter --
      if Sawmill_Supply > 0 then
        Put ("Do the "); Put_Name; Put_Line (" still have" &
                  Recruiter (I)'Image & " recruiter(s) in clearing" &
                  I'Image & "? (y/n)");
        if Get_YN then
          Put ("How many recruiters remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, M.Clearings (I).Buildings);
            Recruiter (I) := Val;
          end;
        end if;
      end if;
    end loop;
  end Warriors_Lost;

  procedure Battle (S : Suit; M : Map) is
    Lost : Integer;
  begin
    for I in Priority'Range loop
      if M.Clearings (I).C_Suit = S and Meeples (I) > 0 then
        Put_Line ("Battle in clearing" & I'Image & " the enemy with " &
                  "the most pieces, then the most points.");
        Put_Line ("How many pieces were lost: ");
        Get_Input (Lost, 0, Meeples(I));
        Meeples (I) := Meeples (I) - Lost;
      end if;
    end loop;
  end Battle;

  procedure Recruit (S : Suit; M : Map) is
    Rule : array (Integer range 1..4) of Integer := (others => 0);
    Count  : Integer range 0..4 := 0;
  begin
    for I in Priority'Range loop
      if M.Clearings (I).C_Suit = S and Meeples (I) > 0 then
        Put ("Do the "); Put_Name; Put_Line (" rule clearing" & I'Image & 
             "? (y/n)");
        if Get_YN then
          Count := Count + 1;
          Rule (Count) := I;
        end if;
      end if;
    end loop;

    case Count is
      when 0 => Put ("The "); Put_Name; Put_Line (" cannot place any warriors!");
      when 1 => Put_Line ("Place 4 warriors in clearing" & Rule (1)'Image);
      when 2 => Put_Line ("Place 2 warriors in clearing" & Rule (1)'Image);
                Put_Line ("Place 2 warriors in clearing" & Rule (2)'Image);
      when 3 => Put_Line ("Place 2 warriors in clearing" & Rule (1)'Image);
                Put_Line ("Place 1 warrior in clearing" & Rule (2)'Image);
                Put_Line ("Place 1 warrior in clearing" & Rule (3)'Image);
      when 4 => 
        for I in Rule'Range loop
          Put_Line ("Place 1 warrior in clearing" & Rule (I)'Image);
        end loop;
    end case;

  end Recruit;

  function Build (S : Suit; M : Map) return Boolean is
    Max : Integer := 0;
    Max_Idx : Integer;
  begin
    for I in Priority'Range loop
      if M.Clearings (I).C_Suit = S and Meeples (I) > 0 then
        Put ("Do the "); Put_Name; Put_Line (" rule clearing" & I'Image & 
                  "? (y/n)");
        if Get_YN then
          Put_Line ("Are there available building slots in clearing" & I'Image & 
                    "? (y/n)");
          if Get_YN then
            Max := Meeples (I);
            Max_Idx := I;
          end if;
        end if;
      end if;
    end loop;

    if Max = 0 then
      Put ("The "); Put_Name; Put_Line (" cannot place any buildings.");
      return False;
    end if;

    case S is
      when Fox    => 
        if Sawmill_Supply /= 0 then
          Put ("Place a SAWMILL in clearing" & Max_Idx'Image);
          Sawmill_Supply := Sawmill_Supply - 1;
        else
          Put ("The "); Put_Name; Put_Line (" cannot place any buildings.");
          return False;
        end if;
      when Rabbit => 
        if Workshop_Supply /= 0 then
          Put ("Place a WORKSHOP in clearing" & Max_Idx'Image);
          Workshop_Supply := Workshop_Supply - 1;
        else
          Put ("The "); Put_Name; Put_Line (" cannot place any buildings.");
          return False;
        end if;
      when Mouse  => 
        if Recruiter_Supply /= 0 then
          Put_Line ("Place a RECRUITER in clearing" & Max_Idx'Image);
          Recruiter_Supply := Recruiter_Supply - 1;
        else
          Put ("The "); Put_Name; Put_Line (" cannot place any buildings.");
          return False;
        end if;
      when Bird   => 
        Put_Line ("WE SHOULD NEVER GET HERE");
    end case;

    return True;
  end Build;

  procedure Move (S : Suit; M : Map) is
  begin
    -- TODO Implement
    null;
  end Move;

end Root.Marquise;

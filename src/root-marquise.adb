with Ada.Text_IO; use Ada.Text_IO;

with Root.Color; use Root.Color;

package body Root.Marquise is

  procedure Put_Name (NewLine : Boolean := False) is
  begin
    Set_Color (Orange);
    Put ("Mechanical Marquise 2.0");
    Reset_Style;
    if NewLine then
      New_Line;
    end if;
  end Put_Name;

  -------------------
  -- Faction Setup --
  -------------------

  function Setup (Clearing : Priority; Diff : Difficulty;
                  M : Map_T) return Boolean is
  begin
    if Clearing > 4 then
      Put_Line ("Invalid starting clearing for Mechanical Marquise 2.0");
      return False;
    end if;

    -- Place starting pieces --
    for I in Meeples'Range loop
      Meeples (I) := 1;
    end loop;
    Meeples (Clearing) := Meeples (Clearing) + 1;

    --if M = Lake_Map then
    --  case Clearing is
    --    when 1 => Meeples (2) := 0;
    --    when 2 => Meeples (1) := 0;
    --    when 3 => Meeples (4) := 0;
    --    when 4 => Meeples (3) := 0;
    --    when others => return False;
    --  end case;
    --else
    case Clearing is
      when 1 => Meeples (3) := 0;
      when 2 => Meeples (4) := 0;
      when 3 => Meeples (1) := 0;
      when 4 => Meeples (2) := 0;
      when others => return False;
    end case;
    --end if;

    return True;
  end Setup;

  ---------------
  -- Take Turn --
  ---------------
  procedure Warriors_Lost (M : Map_T);
  procedure Battle  (S : Suit; M : Map_T);
  procedure Recruit (S : Suit; M : Map_T);
  function  Build   (S : Suit; M : Map_T) return Boolean;
  procedure Move    (S : Suit; M : Map_T);

  procedure Take_Turn (Order : Suit; M : Map_T) is
    Expand : Boolean := True;
  begin

    -- Mechanical Marquise 2.0 State --
    Separator;
    New_Line;
    Set_Color (Orange);
    Put_Line_Center ("Mechanical Marquise 2.0");
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
      Put_Line ("The Mechanical Marquise 2.0 cannot do anything!");
      return;
    end if;

    -- Birdsong --
    Put_Birdsong;
  
    Put_Line ("Craft order card for (+ 1) if it has an available item.");

    Wait_Continue;

    -- Daylight --
    Put_Daylight;

    while Expand loop

      -- Battle --
      Put_Line ("--  Battle");
      Battle (Order, M);

      Wait_Continue;

      -- Recruit --
      Put_Line ("--  Recruit");
      Recruit (Order, M);

      Wait_Continue;

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
    Put_Line (") points for the Mechanical Marquise 2.0");

  end Take_Turn;

  procedure Warriors_Lost (M : Map_T) is
  begin
    -- Determine lost warriors --
    for I in M'Range loop

      -- Warriors --
      if Meeples (I) > 0 then
        Put_Line ("Do the Mechanical Marquise 2.0 still have" &
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
        Put_Line ("Do the Mechanical Marquise 2.0 still have" &
                  Sawmill (I)'Image & " sawmill(s) in clearing" &
                  I'Image & "? (y/n)");
        if Get_YN then
          Put ("How many workshops remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, M (I).Buildings);
            Sawmill (I) := Val;
          end;
        end if;
      end if;

      -- Workshop --
      if Sawmill_Supply > 0 then
        Put_Line ("Do the Mechanical Marquise 2.0 still have" &
                  Workshops (I)'Image & " workshop(s) in clearing" &
                  I'Image & "? (y/n)");
        if Get_YN then
          Put ("How many workshops remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, M (I).Buildings);
            Workshops (I) := Val;
          end;
        end if;
      end if;

      -- Recruiter --
      if Sawmill_Supply > 0 then
        Put_Line ("Do the Mechanical Marquise 2.0 still have" &
                  Recruiter (I)'Image & " recruiter(s) in clearing" &
                  I'Image & "? (y/n)");
        if Get_YN then
          Put ("How many recruiters remain: ");
          declare
            Val : Integer;
          begin
            Get_Input (Val, 0, M (I).Buildings);
            Recruiter (I) := Val;
          end;
        end if;
      end if;
    end loop;
  end Warriors_Lost;

  procedure Battle (S : Suit; M : Map_T) is
    Lost : Integer;
  begin
    for I in M'Range loop
      if M (I).C_Suit = S and Meeples (I) > 0 then
        Put_Line ("Battle in clearing" & I'Image & " the enemy with " &
                  "the most pieces, then the most points.");
        Put_Line ("How many pieces were lost: ");
        Get_Input (Lost, 0, Meeples(I));
        Meeples (I) := Meeples (I) - Lost;
      end if;
    end loop;
  end Battle;

  procedure Recruit (S : Suit; M : Map_T) is
    Rule : array (Integer range 1..4) of Integer := (others => 0);
    Count  : Integer range 0..4 := 0;
  begin
    for I in M'Range loop
      if M (I).C_Suit = S and Meeples (I) > 0 then
        Put_Line  ("Do the Mechanical Marquise 2.0 rule clearing" & I'Image & 
                   "? (y/n)");
        if Get_YN then
          Count := Count + 1;
          Rule (Count) := I;
        end if;
      end if;
    end loop;

    case Count is
      when 0 => Put_Line ("The Mechanical Marquise 2.0 cannot place any warriors!");
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

  function Build (S : Suit; M : Map_T) return Boolean is
    Max : Integer := 0;
    Max_Idx : Integer;
  begin
    for I in M'Range loop
      if M (I).C_Suit = S and Meeples (I) > 0 then
        Put_Line ("Do the Mechanical Marquise 2.0 rule clearing" & I'Image & 
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
      Put_Line ("The Mechanical Marquise 2.0 cannot place any buildings.");
      return False;
    end if;

    case S is
      when Fox    => 
        if Sawmill_Supply /= 0 then
          Put ("Place a SAWMILL in clearing" & Max_Idx'Image);
          Sawmill_Supply := Sawmill_Supply - 1;
        else
          Put_Line ("The Mechanical Marquise 2.0 cannot place any buildings.");
          return False;
        end if;
      when Rabbit => 
        if Workshop_Supply /= 0 then
          Put ("Place a WORKSHOP in clearing" & Max_Idx'Image);
          Workshop_Supply := Workshop_Supply - 1;
        else
          Put_Line ("The Mechanical Marquise 2.0 cannot place any buildings.");
          return False;
        end if;
      when Mouse  => 
        if Recruiter_Supply /= 0 then
          Put ("Place a RECRUITER in clearing" & Max_Idx'Image);
          Recruiter_Supply := Recruiter_Supply - 1;
        else
          Put_Line ("The Mechanical Marquise 2.0 cannot place any buildings.");
          return False;
        end if;
      when Bird   => 
        Put ("WE SHOULD NEVER GET HERE");
    end case;

    return True;
  end Build;

  procedure Move (S : Suit; M : Map_T) is
  begin
    -- TODO Implement
    null;
  end Move;

end Root.Marquise;

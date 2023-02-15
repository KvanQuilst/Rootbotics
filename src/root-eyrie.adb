with Ada.Text_IO; use Ada.Text_IO;

package body Root.Eyrie is

  -- Prompt to place a number of warriors at a specific clearing --
  -- Check for 0 supply included                                 --
  procedure Place_Warriors (Num_Warriors : Integer; Clearing : Priority) is
    N : Integer := Num_Warriors;
  begin
    if Meeple_Supply = 0 then
      return;
    elsif Num_Warriors > Meeple_Supply then
      N := Meeple_Supply;
    end if;
    Put_Line ("Place" & N'Image & " warriors in clearing" &
              Clearing'Image);
    Meeple_Supply := Meeple_Supply - N;
  end Place_Warriors;

  
  -- Recruit phase of daylight --
  procedure Recruit (S : Suit; M : Map_T) is
    Clearings : array (Integer range 1..7) of Integer range 0..12 := (others => 0);
    C_Idx : Integer range 1..7;
    Option : Character;
  begin
    -- Find Clearings with Roosts
    C_Idx := 1;
    for J in M'Range loop
      -- Fox, Mouse, Rabbit Decrees --
      if M (J).C_Suit = S and Roosts (J) then
        Clearings (C_Idx) := J;
        C_Idx := C_Idx + 1;

      -- Bird Decrees --
      elsif S = Bird and Roosts (J) then
        Clearings (C_Idx) := J;
        C_Idx := C_Idx + 1;
      end if;
    end loop;

    -- Multiple clearings to pick from
    if C_Idx > 2 then
      Put_Line ("Which clearing has the most enemies: ");
      Put_Line ("--------------------");
      C_Idx := 1;
      for J in Clearings'Range loop
        if Clearings (J) /= 0 then
          Put_Line (" " & Character'Val (96 + C_Idx) & "." & Clearings (J)'Image); 
          C_Idx := C_Idx + 1;
        end if;
      end loop;
      Put_Line (" " & Character'Val (96 + C_Idx) & ". multiple");
      C_Idx := C_Idx + 1;
      Put_Line (" " & Character'Val (96 + C_Idx) & ". no enemies");
      Put_Line ("--------------------");

      Get_Input (Option, C_Idx);

      -- No enemies --
      if Character'Pos (Option) - 96 = C_Idx then
        declare
          Min : Integer := 20;
          Min_P : Integer := 1;
        begin
          -- Inherently accounts for priority --
          for J in Roosts'Range loop
            if Roosts (J) and Meeples (J) <= Min then
              Min := Meeples (J);
              Min_P := J;
            end if;
          end loop;
        
          Put (S'Image & ": ");
          Place_Warriors (Decrees (S), Clearings (Min_P));
        end;

      -- Multiple clearings with most enemies --
      elsif Character'Pos (Option) - 96 = C_Idx - 1 then
        Put_Line ("Implement: Place warriors in clearing with most enemies, " &
                  "then few Eyrie warriors, then lowest priority");
        -- Choose multiple clearings, pick lowest eyrie warriors/priority

      else -- One clearing with enemies --
        C_Idx := Character'Pos (Option) - 96;
        Put (S'Image & ": ");
        Place_Warriors (Decrees (S), Clearings (C_Idx));
      end if;

    -- Only 1 clearing to pick from
    elsif C_Idx = 2 then
      Put (S'Image & ": ");
      Place_Warriors (Decrees (S), Clearings (1));
      Meeples (Clearings (1)) := Meeples (Clearings (1)) + Decrees (S);

    -- Else no clearings
    end if;
  end Recruit;

  procedure Move (S : Suit; M : Map_T) is
    Max : Integer := 0;
    Max_Idx : Integer;
  begin
    Put_Line (S'Image & ": Unimplemented!");
    return;

    for I in M'Range loop
      -- Inherently accounts for priority --
      if Meeples (I) > Max and M (I).C_Suit = S then
        Max := Meeples (I);
        Max_Idx := I;
      end if;
    end loop;

    if Max = 0 then
      return;
    end if;

  end Move;

  -------------------
  -- Faction Setup --
  -------------------

  function Setup (Clearing : Priority; Diff : Difficulty) return Boolean is
  begin
    if Clearing > 4 then
      Put_Line ("Invalid starting clearing for Electric Eyrie");
      return False;
    end if;

    -- Place Starting Pieces --
    Meeples (Clearing) := 6;
    Roosts  (Clearing) := True;

    return True;
  end Setup;

  ---------------
  -- Take Turn --
  ---------------
  
  procedure Take_Turn (Order : Suit; M : Map_T) is
    Max : Suit;
  begin

    -- Electric Eyrie Stats --
    Put_Line ("--------------------");
    New_Line;
    Put_Line ("   Electric Eyrie   ");
    New_Line;
    Put_Line (" Meeple Supply:" & Meeple_Supply'Image);
    Put_Line ("  Roost Supply:" & Roost_Supply'Image);
    New_Line;
    Put_Line (" Decrees: F  M  R  B");
    Put_Line ("         " & Decrees (Fox)'Image & " "
                          & Decrees (Mouse)'Image & " "
                          & Decrees (Rabbit)'Image & " "
                          & Decrees (Bird)'Image);
    Put_Line ("--------------------");

    if Meeple_Supply = MEEPLE_MAX and Roost_Supply = ROOST_MAX then
      Put_Line ("The Electric Eyrie cannot do anything!");
      return;
    end if;

    -- Birdsong --
    New_Line; 
    Put_Line ("--------------------");
    Put_Line ("      Birdsong      ");
    Put_Line ("--------------------");
    New_Line;
    
    Put_Line ("Craft order card for (+ 1) if it has an available item.");
    Decrees (Order) := Decrees (Order) + 1;

    -- Daylight --
    New_Line;
    Put_Line ("--------------------");
    Put_Line ("      Daylight      ");
    Put_Line ("--------------------");
    New_Line;
    
    Max := Fox;
    for I in Decrees'Range loop
      if Decrees(I) > Decrees(Max) then
         Max := I;
      end if;
    end loop;
    
      -- Recruit --
    Put_Line ("--  Recruit");
    for I in Decrees'Range loop
      if Decrees (I) > 0 and Meeple_Supply > 0 then
        Recruit (I, M);        
      end if;      
    end loop;
    New_Line;

      -- Move --
    Put_Line ("--  Move");
      for I in Decrees'Range loop
        if Decrees (I) > 0 then
          Move (I, M);
        end if;
      end loop;
    New_Line;
    
      -- Battle --

      -- Build --

    -- Evening --
    New_Line;
    Put_Line ("--------------------");
    Put_Line ("       Evening      ");
    Put_Line ("--------------------");
    New_Line;

    Put ("Score (+" & Roost_Points'Image & ") points for the Electric Eyrie.");

  end Take_Turn;

end Root.Eyrie;

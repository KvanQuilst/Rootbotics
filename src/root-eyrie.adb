with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Root.Eyrie is

  procedure Put_Logo is
    Length : constant := 22;
  begin
    Set_Style (B_Blue);
    Put_Line_Centered ("                _     ");
    Put_Line_Centered ("             /|/ |    ");
    Put_Line_Centered ("           /     |/|  ");
    Put_Line_Centered ("         /   /|-   |_ ");
    Put_Line_Centered ("      // ____       / ");
    Put_Line_Centered ("   /    /  / \      / ");
    Put_Line_Centered (" /      \__\_/      //");
    Put_Line_Centered ("| ____              //");
    Reset_Style;
    Put (To_String ((WIDTH - Length) / 2 * "-"));
    Set_Style (B_Blue);
    Put               ("|/------------------>>");
    Reset_Style;
    Put_Line (To_String ((WIDTH - Length) / 2 * "-"));
  end Put_Logo;

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

  -------------------
  -- Faction Setup --
  -------------------

  procedure Setup is
    Corner : Integer range 1..4;
  begin
    Put_Line ("Which corner clearing will the " & Name & " start in: ");
    Corner := Get_Integer (1, 4);

    -- Place Starting Pieces --
    Meeples (Corner) := 6;
    Roosts  (Corner) := True;

  end Setup;

  ---------------
  -- Take Turn --
  ---------------
  procedure Recruit (S : Suit; M : Map);
  procedure Move    (S : Suit; M : Map); 
  procedure Battle  (S : Suit; M : Map; Most : Boolean);

  procedure Take_Turn (Order : Suit; M : Map) is
    Max : Suit;
    Tie : Boolean := False;
  begin

    -- Electric Eyrie Stats --
    Put_Logo;
    New_Line;
    Put_Line_Centered (Name);
    New_Line;
    Put_Line (" Meeple Supply:" & Meeple_Supply'Image);
    Put_Line ("  Roost Supply:" & Roost_Supply'Image);
    New_Line;
    Put_Line (" Decrees: F  M  R  B");
    Put_Line ("         " & Decrees (Fox)'Image & " "
                          & Decrees (Mouse)'Image & " "
                          & Decrees (Rabbit)'Image & " "
                          & Decrees (Bird)'Image);
    New_Line;
    Put (" Current Order: ");
    case Order is
      when Fox    => Put_Line (Root.IO.Fox);
      when Mouse  => Put_Line (Root.IO.Mouse);
      when Rabbit => Put_Line (Root.IO.Rabbit);
      when Bird   => Put_Line (Root.IO.Bird);
    end case;
    New_Line;
    Separator;

    -- TODO Determine lost warriors --

    if Meeple_Supply = MEEPLE_MAX and Roost_Supply = ROOST_MAX then
      New_Line;
      Put_Line ("The " & Name & " cannot do anything!");
      return;
    end if;

    -- Birdsong --
    Put_Birdsong; 

    Put_Line ("Craft order card for (+ 1) if it has an available item.");
    Decrees (Order) := Decrees (Order) + 1;

    Continue;

    -- Daylight --
    Put_Daylight; 

    Max := Fox;
    for I in Decrees'Range loop
      if I /= Max and Decrees (I) = Decrees (Max) then
        Tie := True;
      elsif Decrees (I) > Decrees (Max) then
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

    Continue;

      -- Move --
    Put_Line ("--  Move");
    for I in Decrees'Range loop
      if Decrees (I) > 0 then
        Move (I, M);
      end if;
    end loop;

    Continue;
    
      -- Battle --
    Put_Line ("-- Battle");
    for I in Decrees'Range loop
      if Decrees (I) > 0 then
        Battle (I, M, Max = I and not Tie);
      end if;
    end loop;

      -- Build --

    -- Evening --
    Put_Evening;

    Put_Line ("Score (+" & Roost_Points'Image & ") points for the " & Name);

  end Take_Turn;

  -- Recruit phase of daylight --
  procedure Recruit (S : Suit; M : Map) is
    Clearings : array (Integer range 1..7) of Integer range 0..12 := (others => 0);
    C_Idx : Integer range 1..7;
    Option : Character;
  begin
    -- Find Clearings with Roosts
    C_Idx := 1;
    for J in Priority'Range loop
      -- Fox, Mouse, Rabbit Decrees --
      if M.Clearings (J).C_Suit = S and Roosts (J) then
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
      declare
        Options : String_Arr (Priority'Range);
      begin
        Put_Line ("Which clearing has the most enemies: ");
        C_Idx := 1;
        for J in Clearings'Range loop
          if Clearings (J) /= 0 then
            Options (C_Idx) := To_Unbounded_String (Clearings (J)'Image);
            C_Idx := C_Idx + 1;
          end if;
        end loop;
        Options (C_Idx) := To_Unbounded_String ("multiple");
        C_Idx := C_Idx + 1;
        Options (C_Idx) := To_Unbounded_String ("no enemies");

        Option := Get_Option (Options (1..C_Idx));
      end;

      -- No enemies --
      if Character'Pos (Option) - 96 = C_Idx then
        declare
          Min : Integer := MEEPLE_MAX;
          Min_P : Integer := 1;
        begin
          -- Inherently accounts for priority --
          for J in Roosts'Range loop
            if M.Clearings (J).C_Suit = S and Roosts (J) and Meeples (J) <= Min then
              Min := Meeples (J);
              Min_P := J;
            end if;
          end loop;
        
          Put (S'Image & ": ");
          Place_Warriors (Decrees (S), Clearings (Min_P));
        end;

      -- Multiple clearings with most enemies --
      elsif Character'Pos (Option) - 96 = C_Idx - 1 then
        -- Choose multiple clearings, pick lowest eyrie warriors/priority
        declare
          PL : Priority_List;
          Min : Integer := MEEPLE_MAX;
          Min_P : Integer := 1;
        begin
          PL := Get_List ("clearings with the most enemies");
          for I in PL'Range loop
            if PL (I) /= 0 then
              if M.Clearings (PL (I)).C_Suit = S and Roosts (PL (I)) and Meeples (PL (I)) <= Min then
                Min := Meeples (PL (I));
                Min_P := PL (I);
              end if;
            end if;
          end loop;
        end;

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

  -- Move phase of daylight --
  procedure Move (S : Suit; M : Map) is
    Clearings : Priority_List := (others => 0);
    Count : Integer := 0;
    Max, Max_Idx : Integer := 0;
    Min, Min_R : Integer := MEEPLE_MAX;
    Min_Idx, Min_RIdx : Integer := 0;
    Val : Integer range 0..MEEPLE_MAX;

    Options : String_Arr (Priority'Range);
  begin
    -- Find matching clearings with warriors --
    for I in Priority'Range loop
      if M.Clearings (I).C_Suit = S and Meeples (I) > 0 then
        Count := Count + 1;
        Options (I) := To_Unbounded_String (I'Image);
        Clearings (Count) := I;
      end if;
    end loop;

    if Count = 0 then
      return;
    end if;

    -- Print options --
    Put_Line ("Which of these " & S'Image & " clearings do you rule:");
    declare
      Opts : Char_Arr := Get_Options (Options (1..Count));
    begin
      if Opts'Length = 0 then
        return;
      end if;

      -- Find clearing with most warriors and highest priority --
      for I in Opts'Range loop
        Val := Clearings (Character'Pos (Opts (I)) - 96);

        if Meeples (Val) > Max and Val > Max_Idx then
          Max := Meeples (Val);
          Max_Idx := Val;
        end if;
      end loop;
    end;
      
    -- If there's warriors to move... --
    if Max > 0 then
      for I in M.Clearings (Max_Idx).Neighbors'Range loop
        if M.Clearings (Max_Idx).Neighbors (I) /= 0 then
          Put_Line ("What is the total number of enemy PIECES in clearing" &
                    M.Clearings (Max_Idx).Neighbors (I)'Image & ": ");
          Val := Get_Integer (0, 30);

          -- Track for all neighbors --
          if Val <= Min then
            Min := Val;
            Min_Idx := M.Clearings (Max_Idx).Neighbors (I);
          end if;

          -- Track for neighbors without roosts --
          if not Roosts (M.Clearings (Max_Idx).Neighbors (I)) and
             Val <= Min then
            Min_R := Val;
            Min_RIdx := M.Clearings (Max_Idx).Neighbors (I);
          end if;
        end if;
      end loop;

      Min_Idx := (if Min_RIdx /= 0 then Min_RIdx else Min_Idx);

      Put_Line ("How many warriors do the " & Name & " need to rule clearing" &
           Max_Idx'Image & "? ");
      Val := Get_Integer (0, 30);
      -- TODO Check if eyrie remains in rule if moved --
      Val := (if Val > Decrees (S) then Val else Decrees (S));
      Val := Meeples (Max_Idx) - Val;
      Put_Line ("Move" & Val'Image & " warriors from clearing" & Max_Idx'Image &
                " to clearing" & Min_Idx'Image);
    end if;
  end Move;

  procedure Battle (S : Suit; M : Map; Most : Boolean) is
    Clearings : array (Integer range 1..4) of Integer range 0..12 := (others => 0);
    Count : Integer := 0;
  begin
    -- Find matching clearings with warriors --
    for I in Priority'Range loop
      if M.Clearings (I).C_Suit = S and Meeples (I) > 0 then
        Count := Count + 1;
        Clearings (Count) := I;
      end if;
    end loop;

    if Count = 0 then
      return;
    end if;

    declare
      Enemy_Build : array (Integer range 1..Count) of Integer range 0..3;
    begin
      -- Determine which clearing has the most enemy buildings --
      for I in Enemy_Build'Range loop
        Put_Line ("How many buildings does the enemy with the most buildings" &
             " have in clearing" & Clearings (I)'Image & ": ");
        Enemy_Build (I) := Get_Integer (0, 3);
      end loop;
    end;

  end Battle;

end Root.Eyrie;

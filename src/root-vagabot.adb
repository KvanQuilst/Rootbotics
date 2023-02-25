package body Root.Vagabot is
  
  procedure Add_Item is
  begin
    Undamaged_Idx := Undamaged_Idx + 1;
    Undamaged (Undamaged_Idx) := Unexhausted;
  end Add_Item;

  procedure Exhaust_Item is
    I : Item_Idx := Undamaged_Idx;
  begin
    if Undamaged_Idx = 0 then
      return;
    end if;

    -- Exhaust greatest idx'd unexhausted --
    while I > 0 loop
      if Undamaged (I) = Unexhausted then
        Undamaged (I) := Exhausted;
        return;
      end if;

      I := I - 1;
    end loop;
  end Exhaust_item;

  procedure Refresh_Item is
    I : Item_Idx := 1;
  begin
    if Undamaged_Idx = 0 then
      return;
    end if;

    -- Refresh the least idx'd exhausted --
    while I <= Undamaged_Idx loop
      if Undamaged (I) = Exhausted then
        Undamaged (I) := Unexhausted;
        return;
      end if;

      I := I + 1;
    end loop;
  end Refresh_Item;
  
  procedure Damage_Item is
  begin
    if Undamaged_Idx = 0 then
      return;
    end if;

    Damaged_Idx := Damaged_Idx + 1;
    Damaged (Damaged_Idx) := Undamaged (Undamaged_Idx);
    Undamaged (Undamaged_Idx) := Empty;
    Undamaged_Idx := Undamaged_Idx - 1;
  end Damage_Item;

  procedure Repair_Item is
    I : Item_Idx := Damaged_Idx;
  begin
    if Damaged_Idx = 0 then
      return;
    end if;

    Undamaged_Idx := Undamaged_Idx + 1;
    Undamaged (Undamaged_Idx) := Exhausted;

    -- Find greatest idx'd unexhausted item --
    while I > 0 loop
      if Damaged (I) = Unexhausted then
        Refresh_Item;                         -- Repaired item is unexhausted
        Damaged (I) := Damaged (Damaged_Idx); -- Move last entry to I
      end if;

      I := I - 1;
    end loop;

    -- else, everything is exhausted... --

    Damaged (Damaged_Idx) := Empty;
    Damaged_Idx := Damaged_Idx - 1;
  end Repair_Item;

  -------------------
  -- Faction Setup --
  -------------------

  function Setup (Char : V_Character; Diff : Difficulty) return Boolean is
  begin
    if Char = Tinker then
      Num_Items := 3;
    end if;

    for I in 1..Num_Items loop
      Add_Item;
    end loop;

    return True;
  end Setup;


  ---------------
  -- Take Turn --
  ---------------
  
  procedure Take_Turn (Order : Suit; M : Map_T) is
  begin

    -- Vagabot Stats --
     

    -- Birdsong --

    -- Daylight --

    -- Evening --
  end Take_Turn;

end Root.Vagabot;

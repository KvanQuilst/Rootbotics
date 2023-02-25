with Ada.Text_IO; use Ada.Text_IO;

package body Root.Vagabot is
  
  -------------------------------
  -- Character Special Actions --
  -------------------------------

  procedure Thief_Special is
  begin
    Put_Line ("Thief_Special unimplemented!");
  end Thief_Special;

  procedure Tinker_Special is
  begin
    Put_Line ("Tinker_Special unimplemented!");
  end Tinker_Special;

  procedure Ranger_Special is
  begin
    Put_Line ("Ranger_Special unimplemented!");
  end Ranger_Special;

  procedure Vagrant_Special is
  begin
    Put_Line ("Vagrant_Special unimplemented!");
  end Vagrant_Special;

  procedure Scoundrel_Special is
  begin
    Put_Line ("Scoundrel_Special unimplemented!");
  end Scoundrel_Special;

  procedure Arbiter_Special is
  begin
    Put_Line ("Arbiter_Special unimplemented!");
  end Arbiter_Special;

  ---------------------
  -- Item Management --
  ---------------------

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


  function Exhausted (Arr : Item_Arr) return Integer is
    Cnt : Integer := 0;
  begin
    for I in Arr'Range loop
      if Arr (I) = Exhausted then
        Cnt := Cnt + 1;
      end if;
    end loop;

    return Cnt;
  end Exhausted;

  -------------------
  -- Faction Setup --
  -------------------

  function Setup (Char : V_Character; Diff : Difficulty) return Boolean is
  begin
    case Char is
      when Thief =>
        Special := Thief_Special'Access;

      when Tinker =>
        Special := Tinker_Special'Access;
        Num_Items := 3;

      when Ranger =>
        Special := Ranger_Special'Access;

      when Vagrant =>
        Special := Vagrant_Special'Access;

      when Scoundrel =>
        Special := Scoundrel_Special'Access;

      when Arbiter =>
        Special := Arbiter_Special'Access;
    end case;

    for I in 1..Num_Items loop
      Add_Item;
    end loop;

    return True;
  end Setup;


  ---------------
  -- Take Turn --
  ---------------
  procedure Explore;
  procedure Quest;
  procedure Aid;
  procedure Battle;
  
  procedure Take_Turn (Order : Suit; M : Map_T) is
  begin

    -- Vagabot Stats --
    Separator; 
    New_Line;
    Put_Line_Center ("Vagabot");
    New_Line;
    Put_Line ("           Undamaged Items:" & Undamaged_Idx'Image);
    Put_Line (" Exhausted Undamaged Items:" & Exhausted (Undamaged)'Image);
    New_Line;
    Put_Line ("             Damaged Items:" & Damaged_Idx'Image);
    Put_Line ("   Exhausted Damaged Items:" & Exhausted (Damaged)'Image);
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

  procedure Explore is
  begin
    null;
  end Explore;

  procedure Quest is
  begin
    null;
  end Quest;

  procedure Aid is
  begin
    null;
  end Aid;

  procedure Battle is
  begin
    null;
  end Battle;

end Root.Vagabot;

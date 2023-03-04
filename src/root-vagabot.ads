with Root.Maps; use Root.Maps;

package Root.Vagabot is

  
  procedure Setup;
  procedure Take_Turn (Order : Suit; M : Map);
  procedure Put_Name (NewLine : Boolean := False);
  function  Name return String;

private

  MAX_ITEMS : constant := 20;

  Num_Items : Positive := 4;

  type V_Character is (Thief, Tinker, Ranger, Vagrant, Scoundrel, Arbiter);

  type Item_State  is (Exhausted, Unexhausted, Empty);
  type Item_Arr    is array (Integer range 1..MAX_ITEMS) of Item_State;
  subtype Item_Idx is Integer range 0..MAX_ITEMS;

  Special : access procedure;

  -- Order: [Unexhausted items, Exhausted items] --
  Undamaged : Item_Arr := (others => Empty);
  Undamaged_Idx : Item_Idx;

  -- Order: None --
  Damaged   : Item_Arr := (others => Empty);
  Damaged_Idx : Item_Idx;

end Root.Vagabot;

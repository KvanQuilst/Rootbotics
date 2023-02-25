with Root.Map; use Root.Map;

package Root.Vagabot is

  type V_Character is (Thief, Tinker, Ranger, Vagrant, Scoundrel, Arbiter);
  
  function Setup (Char : V_Character; Diff : Difficulty) return Boolean;
  procedure Take_Turn (Order : Suit; M : Map_T);

private

  MAX_ITEMS : constant := 20;

  Num_Items : Positive := 4;

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

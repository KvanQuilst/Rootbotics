with Root.Map; use Root.Map;

package Root.Eyrie is

  function  Setup (Clearing : Priority; Diff : Difficulty) return Boolean;
  procedure Take_Turn (Order : Suit; M : Map_T);

private

  MEEPLE_MAX : constant := 20;
  ROOST_MAX  : constant := 7;

  Meeple_Supply : Integer range 0..MEEPLE_MAX := MEEPLE_MAX - 6;
  Roost_Supply  : Integer range 0..ROOST_MAX  := ROOST_MAX - 1;

  Meeples : array (Priority'Range) of Natural;
  Roosts  : array (Priority'Range) of Boolean;
  Decrees : array (Suit) of Natural := (Bird => 2, others => 0);
  Roost_Points  : Integer range 0..6 := 0;

end Root.Eyrie;

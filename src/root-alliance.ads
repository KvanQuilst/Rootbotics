with Root.Map; use Root.Map;

package Root.Alliance is

  function Setup (Diff : Difficulty) return Boolean;
  procedure Take_Turn (Order : Suit; M : Map_T);
  procedure Put_Name (NewLine : Boolean := False);

private

  MEEPLE_MAX : constant := 10;
  SYMPATHY_MAX : constant := 10;

  Meeple_Supply   : Integer range 0..MEEPLE_MAX  := MEEPLE_MAX;
  Sympathy_Supply : Integer range 0..SYMPATHY_MAX := SYMPATHY_MAX;

  Meeples    : array (Priority'Range) of Natural;
  Sympathies : array (Priority'Range) of Boolean;

end Root.Alliance;

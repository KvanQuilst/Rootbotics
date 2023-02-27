with Root.Map; use Root.Map;

package Root.Marquise is

  procedure Setup     (M : Map_T);
  procedure Take_Turn (Order : Suit; M : Map_T);
  procedure Put_Name  (NewLine : Boolean := False);

private

  MEEPLE_MAX    : constant := 25;
  BUILDINGS_MAX : constant := 6;

  subtype Building_Supply is Integer range 0..BUILDINGS_MAX;

  Meeple_Supply    : Integer range 0..MEEPLE_MAX := MEEPLE_MAX - 12;
  Sawmill_Supply   : Building_Supply := BUILDINGS_MAX - 1;
  Workshop_Supply  : Building_Supply := BUILDINGS_MAX - 1;
  Recruiter_Supply : Building_Supply := BUILDINGS_MAX - 1;

  Meeples   : array (Priority'Range) of Natural;
  Sawmill   : array (Building_Supply'Range) of Priority;
  Workshops : array (Building_Supply'Range) of Priority;
  Recruiter : array (Building_Supply'Range) of Priority;

end Root.Marquise;
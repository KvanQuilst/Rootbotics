with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package Root.Marquise is

   Name : constant String := String_Style ("Mechanical Marquise 2.0", Yellow);

   procedure Setup     (M : Map);
   procedure Take_Turn (Order : Suit; M : Map);

private

   MEEPLE_MAX    : constant := 25;
   BUILDINGS_MAX : constant := 6;

   subtype Building_Supply is Integer range 0 .. BUILDINGS_MAX;

   Meeple_Supply    : Integer range 0 .. MEEPLE_MAX := MEEPLE_MAX - 12;
   Sawmill_Supply   : Building_Supply := BUILDINGS_MAX - 1;
   Workshop_Supply  : Building_Supply := BUILDINGS_MAX - 1;
   Recruiter_Supply : Building_Supply := BUILDINGS_MAX - 1;

   Meeples   : array (Priority'Range) of Integer range 0 .. MEEPLE_MAX;
   Sawmill   : array (Priority'Range) of Integer range 0 .. 3;
   Workshops : array (Priority'Range) of Integer range 0 .. 3;
   Recruiter : array (Priority'Range) of Integer range 0 .. 3;

end Root.Marquise;

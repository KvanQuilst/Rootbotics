with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package Root.Marquise is

   Name : constant String := String_Style ("Mechanical Marquise 2.0", Yellow);

   procedure Setup     (M : Map);
   procedure Take_Turn (Order : Suit; M : Map);

private

   MEEPLE_MAX    : constant := 25;
   BUILDINGS_MAX : constant := 6;

   Meeple_Supply    : Integer range 0 .. MEEPLE_MAX := MEEPLE_MAX - 12;

   Meeples   : array (Priority'Range) of Integer range 0 .. MEEPLE_MAX;
   Rule      : array (Priority'Range) of Boolean;

   type Building is (Sawmill, Workshop, Recruiter);

   Buildings : array (Building'Range, Priority'Range) of
     Integer range 0 .. 3;

   Building_Supply : array (Building'Range) of
      Integer range 0 .. BUILDINGS_MAX;

end Root.Marquise;

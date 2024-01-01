with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package Root.Lizards is

   Faction_Color : constant Color := B_Yellow;
   Name : constant String := String_Style ("Logical Lizards", Faction_Color);

   procedure Take_Turn (M : Map);
   procedure Setup;

private

   MEEPLE_MAX  : constant Integer := 25;
   GARDENS_MAX : constant Integer := 5;

   subtype Garden is Suit range Fox .. Rabbit;
   type Conspiracy is (Convert, Crusade, Sanctify);
   type Conspiracy_Count is mod 5;

   Meeple_Supply : Integer range 0 .. MEEPLE_MAX := MEEPLE_MAX;
   Map_Warriors  : Meeple_Arr;
   Rule          : array (Priority'Range) of Boolean;

   Acolytes        : Integer range 0 .. MEEPLE_MAX := 0;
   Conspiracies    : constant array (Conspiracy_Count'Range) of Conspiracy :=
                        (Convert, Crusade, Convert, Crusade, Sanctify);
   Next_Conspiracy : Conspiracy_Count := 0;

   Garden_Supply : array (Garden'Range) of Integer range 0 .. GARDENS_MAX;
   Gardens       : array (Priority'Range, Integer range 1 .. 3) of Garden;

   Curr_Order : Suit;

   function Unbounded (S : String) return Unbounded_String
      renames To_Unbounded_String;
   Logo_Width : constant := 24;
   Logo : constant array (Integer range <>) of Unbounded_String :=
      (Unbounded (String_Style ("                        ", Faction_Color)),
       Unbounded (String_Style ("      ___               ", Faction_Color)),
       Unbounded (String_Style ("    / ___ \___          ", Faction_Color)),
       Unbounded (String_Style (" </  /\  \     \___     ", Faction_Color)),
       Unbounded (String_Style ("<|   \/__/          \^\ ", Faction_Color)),
       Unbounded (String_Style (" | .   ________________\", Faction_Color)),
       Unbounded (String_Style ("<|    ___    | |      / ", Faction_Color)),
       Unbounded (String_Style (" <|  /   \    ^      /  ", Faction_Color)),
       Unbounded (String_Style ("   |_\___/_________/    ", Faction_Color))
      );

end Root.Lizards;

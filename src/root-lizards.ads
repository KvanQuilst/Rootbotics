with Root.IO; use Root.IO;
with Root.Maps; use Root.Maps;

package Root.Lizards is

   Name : constant String := String_Style ("Logical Lizards", B_Yellow);

   procedure Take_Turn (M : Map);

private

   MEEPLE_MAX  : constant Integer := 25;
   GARDENS_MAX : constant Integer := 5;

   subtype Garden is Suit range Fox .. Rabbit;
   type Conspiracy is (Convert, Crusade, Sanctify);
   type Conspiracy_Count is mod 5;

   Meeple_Supply : Integer range 0 .. MEEPLE_MAX := MEEPLE_MAX;
   Meeples       : Meeple_Arr;
   Rule          : array (Priority'Range) of Boolean;

   Acolytes        : Integer range 0 .. MEEPLE_MAX := 0;
   Conspiracies    : constant array (Conspiracy_Count'Range) of Conspiracy :=
                        (Convert, Crusade, Convert, Crusade, Sanctify);
   Next_Conspiracy : Conspiracy_Count := 0;

   Garden_Supply : array (Garden'Range) of Integer range 0 .. GARDENS_MAX;
   Gardens       : array (Priority'Range, Integer range 1 .. 3) of Garden;

   Curr_Order : Suit;

end Root.Lizards;

package Root is

   type Difficulty is (Easy, Default, Challenging, Nightmare);

   type Suit is (Fox, Mouse, Rabbit, Bird);
   subtype Priority is Integer range 1 .. 12;

   type Meeple_Arr is array (Priority'Range) of Natural;
   type Warrior_Arr  is array (Priority'Range) of Natural;
   type Building_Arr is array (Priority'Range) of Integer range 0 .. 3;
   type Rule_Arr     is array (Priority'Range) of Boolean;

   type Priority_List is array (Priority'Range)
     of Integer range 0 .. 12;

   type Help_Procedure is access procedure;
   Help : Help_Procedure := null;

   function  Get_List (Values : String) return Priority_List;

end Root;

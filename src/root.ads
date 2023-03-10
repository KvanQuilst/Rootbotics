package Root is

   type Difficulty is (Easy, Default, Challenging, Nightmare);

   type Suit is (Fox, Mouse, Rabbit, Bird);
   subtype Priority is Integer range 1 .. 12;

   type Priority_List is array (Integer range 1 .. 12)
     of Integer range 0 .. 12;

   type Help_Procedure is access procedure;
   Help : Help_Procedure := null;

   function  Get_List (Values : String) return Priority_List;

end Root;

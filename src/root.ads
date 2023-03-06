package Root is

  type Difficulty is (Easy, Default, Challenging, Nightmare);

  type Suit is (Fox, Mouse, Rabbit, Bird);
  subtype Priority is Integer range 1..12;

  type Priority_List is array (Integer range 1..12) of Integer range 0..12;
  type Option_List is array (Positive range <>) of Character;

  function  Get_YN     return Boolean;
  function  Get_List   (Values : String) return Priority_List;
  procedure Get_List   (OL : out Option_List; Num_Opts : Positive);

end Root;

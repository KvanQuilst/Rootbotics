package Root is

  type Difficulty is (Easy, Default, Challenging, Nightmare);

  type Suit is (Fox, Mouse, Rabbit, Bird);
  subtype Priority is Integer range 1..12;

  type Priority_List is array (Integer range 1..12) of Integer range 0..12;
  type Option_List is array (Positive range <>) of Character;

  procedure Get_Option (C : out Character; Num_Opts : Integer);
  procedure Get_Input  (I : out Integer);
  function Get_List    (Values : String) return Priority_List;
  procedure Get_List   (OL : out Option_List);

  procedure Wait_Continue;

end Root;

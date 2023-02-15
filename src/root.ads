package Root is

  type Difficulty is (Easy, Default, Challenging, Nightmare);

  type Suit is (Fox, Mouse, Rabbit, Bird);
  subtype Priority is Integer range 1..12;

  type Input_List is array (Positive range <>) of Integer range 0..12;

  procedure Get_Input (C : out Character; Num_Opts : Integer);
  function Get_List  (Values : String) return Input_List;

end Root;

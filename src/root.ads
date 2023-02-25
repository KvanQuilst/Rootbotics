package Root is

  type Difficulty is (Easy, Default, Challenging, Nightmare);

  type Suit is (Fox, Mouse, Rabbit, Bird);
  subtype Priority is Integer range 1..12;

  type Priority_List is array (Integer range 1..12) of Integer range 0..12;
  type Option_List is array (Positive range <>) of Character;

  procedure Get_Option (C : out Character; Num_Opts : Integer);
  function  Get_YN     return Boolean;
  procedure Get_Input  (I : out Integer; Low, High : Integer);
  function  Get_List   (Values : String) return Priority_List;
  procedure Get_List   (OL : out Option_List; Num_Opts : Positive);

  WIDTH : constant := 25;

  procedure Wait_Continue;
  procedure Separator;
  procedure Put_Line_Center (S : String);

  procedure Put_Birdsong;
  procedure Put_Daylight;
  procedure Put_Evening;

end Root;

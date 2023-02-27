package Root.Color is

  type Color is (None, Blue, Green, Teal, Purple, 
                 Orange, Peach, Yellow, Dark_Grey);
  for Color use (
    None      => -1,
    Blue      => 4,
    Green     => 34,
    Teal      => 87,
    Purple    => 99,
    Orange    => 208,
    Peach     => 223,
    Yellow    => 226,
    Dark_Grey => 244
  );

  type Style is (Bold, Italic);

  for Style use (
    Bold   => 1,
    Italic => 3
  );

  procedure Set_Color (FG : Color; BG : Color := None);
  procedure Set_Style (S : Style);
  procedure Reset_Style;

end Root.Color;

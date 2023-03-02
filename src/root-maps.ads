package Root.Maps is

  type Neighbor_Arr is array (Integer range 1..6) of Integer range 0..12;

  type Clearing is record
    C_Suit    : Suit;
    Buildings : Integer range 1..3;     
    Ruins     : Boolean;
    Neighbors : Neighbor_Arr;
  end record;

  type Clearing_Arr is array (Integer range 1..12) of Clearing;
  type Map_Name is (Fall, Winter, Lake, Mountain);

  type Map is record
    Name      : Map_Name;
    Clearings : Clearing_Arr;
  end record;

  ----------
  -- Fall --
  ----------
  Fall_Map : constant Map :=
    (Fall,
    ((Fox,    1, False, (5, 9, 10, 0, 0, 0)),   -- 1
     (Mouse,  2, False, (5, 6, 10, 0, 0, 0)),   -- 2
     (Rabbit, 1, False, (6, 7, 11, 0, 0, 0)),   -- 3
     (Rabbit, 1, False, (8, 9, 12, 0, 0, 0)),   -- 4
     (Rabbit, 2, False, (1, 2, 0, 0, 0, 0)),    -- 5
     (Fox,    1, True,  (2, 3, 11, 0, 0, 0)),   -- 6
     (Mouse,  2, False, (3, 8, 12, 0, 0, 0)),   -- 7
     (Fox,    2, False, (4, 7, 0, 0, 0, 0)),    -- 8
     (Mouse,  2, False, (1, 4, 12, 0, 0, 0)),   -- 9
     (Rabbit, 1, True,  (1, 2, 12, 0, 0, 0)),   -- 10
     (Mouse,  2, True,  (3, 6, 12, 0, 0, 0)),   -- 11
     (Fox,    1, True,  (4, 7, 9, 10, 11, 0)))); -- 12

  function Winter_Map   return Map;
  function Lake_Map     return Map;
  function Mountain_Map return Map;

private

  type Clearing_Suit  is array (Priority'Range) of Suit;
  type Clearing_Order is array (Priority'Range) of Priority;

end Root.Maps;

package Root.Maps is

   type Neighbor_Arr is array (Integer range 1 .. 6)
     of Integer range 0 .. 12;

   type Clearing is record
      C_Suit    : Suit;
      Buildings : Integer range 1 .. 3;
      Ruins     : Boolean;
      Neighbors : Neighbor_Arr;
   end record;

   type Clearing_Arr is array (Priority'Range) of Clearing;
   type Map_Name is (Fall, Winter, Lake, Mountain);

   type Map is record
      Name      : Map_Name;
      Clearings : Clearing_Arr;
   end record;

   Map_Play : Map_Name;

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

   function Winter_Map   return Map
      with Inline;
   function Lake_Map     return Map
      with Inline;
   function Mountain_Map return Map
      with Inline;

   procedure Put_Map (Map : Map_Name; Units : Meeple_Arr);

private

   type Clearing_Suit  is array (Priority'Range) of Suit;
   type Clearing_Order is array (Priority'Range) of Priority;

   type Coordinates is array (Priority'Range, 1 .. 2) of Natural;

   Winter_Map_Set   : Boolean := False;
   Lake_Map_Set     : Boolean := False;
   Mountain_Map_Set : Boolean := False;

   Winter_Map_Actual   : Map;
   Lake_Map_Actual     : Map;
   Mountain_Map_Actual : Map;

   Map_Width : constant := 26;
   type Map_Text is array (Integer range <>) of String (1 .. 26);
   -------------------
   -- Fall Map Data --
   -------------------
   Fall_Clearing_Coords : Coordinates :=
      ((0, 0), (1, 21), (9, 21), (9, 0), (0, 14), (5, 21),
       (9, 14), (9, 7), (5, 0), (1, 7), (5, 14), (5, 7));
   Fall_Map_Height : constant := 12;
   Fall_Map_Width  : constant := Map_Width;
   Fall_Map_Base   : Map_Text :=
      ("@---@---------@---@       ",
       "| _ |  @---@  | _ |--@---@",
       "F---1--| _ |  R---5  | _ |",
       "  |    R--10---------M---2",
       "  |      |             |  ",
       "@---@  @---@  @---@  @---@",
       "| _ |--| _ |--| _ |--| _ |",
       "M---9  F--12  M--11  F---6",
       "  |  //     \\     \\  |  ",
       "@---@  @---@  @---@  @---@",
       "| _ |--| _ |--| _ |--| _ |",
       "R---4  F---8  M---7  R---3");

end Root.Maps;

-------------------------------------------------------------------------------
--                                                                           --
--                           THE ROOTBOTICS TOOL                             --
--                                                                           --
--                            ROOT . MAPS (Spec)                             --
--                                                                           --
--                      Copyright (C) 2024 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the map related components of    --
-- The Rootbotics Tool.                                                      --
--                                                                           --
-- The Rootbotics Tool is free software: you can redistribute it and/or      --
-- modify it under the terms of the GNU General Public License as published  --
-- by the Free Software Foundation, either version 3 of the License, or (at  --
-- your option) any later version.                                           --
--                                                                           --
-- GBADA is distributed in the hope that it will be useful, but WITHOUT ANY  --
-- WARRANTY; wihtout even the implied warranty of MERCHANTABILITY or FITNESS --
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more     --
-- details.                                                                  --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Tool. If not, see <https://www.gnu.org/licenses/>.    --
-------------------------------------------------------------------------------
package Root.Maps is

   type Neighbor_Arr is array (Integer range 1 .. 6)
     of Integer range 0 .. 12;

   type Clearing is record
      C_Suit    : Clearing_Suit;
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

   function Clearings return Clearing_Arr;

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

   procedure Put_Map (Units     : Warrior_Arr;
                      Buildings : Building_Arr;
                      Rule      : Rule_Arr);

private
   type Coordinate is record
      x : Natural;
      y : Natural;
   end record;
   type Coordinates is array (Priority'Range) of Coordinate;

   Map_In_Play : Map_Name := Fall;
   Set_Clearings : array (Priority'Range) of Suit;

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
   Fall_Clearing_Coords : constant Coordinates :=
      ((0, 0), (1, 21), (9, 21), (9, 0), (0, 14), (5, 21),
       (9, 14), (9, 7), (5, 0), (1, 7), (5, 14), (5, 7));
   Fall_Map_Height : constant := 12;
   Fall_Map_Base   : constant Map_Text :=
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

   ---------------------
   -- Winter Map Data --
   ---------------------
   Winter_Clearing_Coords : constant Coordinates :=
      ((0, 0), (0, 21), (8, 21), (8, 0), (0, 7), (0, 14),
       (4, 21), (8, 14), (8, 7), (4, 0), (4, 7), (4, 14));
   Winter_Map_Height : constant := 11;
   Winter_Map_Base   : constant Map_Text :=
      ("@---@  @---@  @---@  @---@",
       "| _ |--| _ |--| _ |--| _ |",
       "*---1  *---5  *---6  *---2",
       "  |  \\            //  |  ",
       "@---@  @---@  @---@  @---@",
       "| _ |  | _ |  | _ |  | _ |",
       "*--10  *--11  *--12  *---7",
       "  |  //  |      |  \\  |  ",
       "@---@  @---@  @---@  @---@",
       "| _ |--| _ |--| _ |--| _ |",
       "*---4  *---9  *---8  *---3");

   -------------------
   -- Lake Map Data --
   -------------------
   Lake_Clearing_Coords : constant Coordinates :=
      ((8, 21), (0, 0), (8, 0), (0, 21), (4, 21), (0, 14),
       (0, 7), (4, 0), (8, 14), (4, 7), (4, 14), (7, 7));
   Lake_Map_Height : constant := 11;
   Lake_Map_Base   : constant Map_Text :=
      ("@---@  @---@  @---@  @---@",
       "| _ |--| _ |--| _ |--| _ |",
       "*---2  *---7  *---6  *---4",
       "  |  \\  |  \\  |      |  ",
       "@---@  @---@  @---@  @---@",
       "| _ |--| _ |  | _ |--| _ |",
       "*---8  *--10  *--11  *---5",
       "  |    @---@           |  ",
       "@---@--| _ |--@---@  @---@",
       "| _ |  *--12  | _ |--| _ |",
       "*---3---------*---9  *---1");

   -----------------------
   -- Mountain Map Data --
   -----------------------
   Mountain_Clearing_Coords : constant Coordinates :=
      ((0, 0), (0, 21), (8, 21), (8, 0), (0, 14), (4, 21),
       (8, 14), (4, 0), (0, 7), (4, 7), (4, 14), (8, 7));
   Mountain_Map_Height : constant := 11;
   Mountain_Map_Base   : constant Map_Text :=
      ("@---@  @---@  @---@  @---@",
       "| _ |--| _ |--| _ |--| _ |",
       "*---1  *---9  *---5  *---2",
       "  |      |  //  |  //  |  ",
       "@-^-@  @-^-@  @-^-@  @-^-@",
       "| _ |  | _ |--| _ |--| _ |",
       "*---8  *--10  *--11  *---6",
       "  |      |  //  |  \\  |  ",
       "@-^-@  @-^-@  @-^-@  @-^-@",
       "| _ |--| _ |--| _ |--| _ |",
       "*---4  *--12  *---7  *---3");
end Root.Maps;

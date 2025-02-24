-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                            ROOT . MAPS (Spec)                             --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification for the map related components of    --
-- The Rootbotics Assistant.                                                 --
--                                                                           --
-- The Rootbotics Assistant is free software: you can redistribute it and/or --
-- modify it under the terms of the GNU General Public License as published  --
-- by the Free Software Foundation, either version 3 of the License, or (at  --
-- your option) any later version.                                           --
--                                                                           --
-- The Rootbotics Assistant is distributed in the hope that it will be       --
-- useful, but WITHOUT ANY WARRANTY; wihtout even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General  --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Assistant. If not, see                                --
-- <https://www.gnu.org/licenses/>.                                          --
-------------------------------------------------------------------------------
with IO_Utils.User_IO; use IO_Utils.User_IO;

with Root.Faction; use Root.Faction;

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

   procedure Init_Map (Name : Map_Name);
   function Clearings return Clearing_Arr;
   function Filter_Clearings (S : Suit) return Int_Arr;

   procedure Put_Map (Units     : Warrior_Arr;
                      Buildings : Building_Arr;
                      Rule      : Rule_Arr;
                      Tokens    : Token_Arr := (others => False));

private
   type Coordinate is record
      x : Natural;
      y : Natural;
   end record;
   type Coordinates is array (Priority'Range) of Coordinate;

   Map_In_Play : Map_Name := Fall;
   Set_Clearings : array (Priority'Range) of Suit;

   Map_Width : constant := 26;
   type Map_Text is array (Integer range <>) of String (1 .. 26);

   Map : Clearing_Arr;

   -------------------
   -- Fall Map Data --
   -------------------
   Fall_Map_Clean : constant Clearing_Arr :=
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
       (Fox,    1, True,  (4, 7, 9, 10, 11, 0))); -- 12

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
   Winter_Map_Clean : constant Clearing_Arr :=
      ((Fox,  1, False, (5, 10, 11, 0, 0, 0)),
       (Fox,  1, False, (6, 7, 12, 0, 0, 0)),
       (Fox,  2, False, (7, 8, 12, 0, 0, 0)),
       (Fox,  2, False, (9, 10, 11, 0, 0, 0)),
       (Fox,  2, False, (1, 6, 0, 0, 0, 0)),
       (Fox,  2, False, (5, 2, 0, 0, 0, 0)),
       (Fox,  1, False, (2, 3, 0, 0, 0, 0)),
       (Fox,  1, True,  (3, 9, 12, 0, 0, 0)),
       (Fox,  1, True,  (4, 8, 11, 0, 0, 0)),
       (Fox, 1, False, (1, 4, 0, 0, 0, 0)),
       (Fox, 2, True,  (1, 4, 9, 0, 0, 0)),
       (Fox, 2, True,  (2, 3, 8, 0, 0, 0)));

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
   Lake_Map_Clean : constant Clearing_Arr :=
      ((Fox,  2, False, (5, 9, 0, 0, 0, 0)),
       (Fox,  1, False, (7, 8, 10, 0, 0, 0)),
       (Fox,  1, False, (8, 9, 12, 0, 0, 0)),
       (Fox,  1, False, (5, 6, 0, 0, 0, 0)),
       (Fox,  2, True,  (1, 4, 11, 0, 0, 0)),
       (Fox,  2, False, (4, 7, 11, 0, 0, 0)),
       (Fox,  1, False, (2, 6, 10, 11, 0, 0)),
       (Fox,  1, False, (2, 3, 10, 0, 0, 0)),
       (Fox,  1, False, (1, 3, 12, 0, 0, 0)),
       (Fox, 2, True,  (2, 7, 8, 0, 0, 0)),
       (Fox, 2, True,  (5, 6, 7, 0, 0, 0)),
       (Fox, 2, True,  (3, 9, 0, 0, 0, 0)));

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
   Mountain_Map_Clean : constant Clearing_Arr :=
      ((Fox,  2, False, (8, 9, 0, 0, 0, 0)),
       (Fox,  2, False, (5, 6, 11, 0, 0, 0)),
       (Fox,  2, False, (6, 7, 11, 0, 0, 0)),
       (Fox,  2, False, (8, 12, 0, 0, 0, 0)),
       (Fox,  1, False, (2, 9, 10, 11, 0, 0)),
       (Fox,  1, False, (2, 3, 11, 0, 0, 0)),
       (Fox,  1, False, (3, 12, 0, 0, 0, 0)),
       (Fox,  1, False, (1, 4, 9, 0, 0, 0)),
       (Fox,  2, True,  (1, 5, 8, 10, 12, 0)),
       (Fox, 1, True,  (5, 9, 11, 12, 0, 0)),
       (Fox, 2, True,  (2, 3, 5, 6, 10, 12)),
       (Fox, 2, True,  (4, 7, 9, 10, 11, 0)));

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

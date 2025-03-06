-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                           ROOT . MAPS (Spec)                              --
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
package Root.Maps is

   subtype Clearing_Suit  is Suit range Fox .. Mouse;
   type    Priority_Suits is array (Priority'Range) of Clearing_Suit;

   ---------------
   -- Map Class --
   ---------------
   type Map (M_Type : Map_Type) is tagged private;

   -- Constructors --
   function New_Map (M_Type : Map_Type) return Map;
   function New_Map (M_Type : Map_Type;
                     Suits  : Priority_Suits) return Map;

   -- Check that each suit has four clearings --
   function Validate_Map (Self : Map) return Boolean;

   procedure Place_Warriors (Self         : in out Map;
                             S            :        Seat;
                             Clearing     :        Priority;
                             Num_Warriors :        UInt8);

   procedure Remove_Warriors (Self         : in out Map;
                              S            :        Seat;
                              Clearing     :        Priority;
                              Num_Warriors :        UInt8);

   function Who_Rules (Self     : Map;
                       Clearing : Priority) return Seat;

private

   --------------
   -- Clearing --
   --------------
   type Total_By_Seat is array (Seat'Range) of UInt8;

   type Clearing is record
      Suit        : Clearing_Suit;
      Builds_Free : UInt8 range 0 .. 3;
      Ruins       : Boolean;
      Ruled_By    : UInt8 range 0 .. Seat'Last := 0;
      Warriors    : Total_By_Seat              := (others => 0);
      Tokens      : Total_By_Seat              := (others => 0);
      Buildings   : Total_By_Seat              := (others => 0);
   end record;

   type Clearings_Arr is array (Priority'Range) of Clearing;

   ---------------
   -- Map Class --
   ---------------
   type Map (M_Type : Map_Type) is tagged record
      Clearings : Clearings_Arr;
   end record;

   ----------
   -- Maps --
   ----------
   Fall_Clearings : constant Clearings_Arr := (
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 1
      (Mouse,  2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 2
      (Rabbit, 1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 3
      (Rabbit, 1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 4
      (Rabbit, 2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 5
      (Fox,    1, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 6
      (Mouse,  2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 7
      (Fox,    2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 8
      (Mouse,  2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 9
      (Rabbit, 1, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 10
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 11
      (Fox,    1, True,
       Ruled_By | Warriors | Tokens | Buildings => <>)  -- 12
   );

   Winter_Clearings : constant Clearings_Arr := (
      (Rabbit, 1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 1
      (Rabbit, 1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 2
      (Rabbit, 2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 3
      (Fox,    2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 4
      (Fox,    2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 5
      (Mouse,  2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 6
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 7
      (Fox,    1, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 8
      (Mouse,  1, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 9
      (Mouse,  1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 10
      (Rabbit, 2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 11
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>)  -- 12
   );

   Lake_Clearings : constant Clearings_Arr := (
      (Rabbit, 2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 1
      (Rabbit, 1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 2
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 3
      (Rabbit, 1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 4
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 5
      (Mouse,  2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 6
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 7
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 8
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 9
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 10
      (Rabbit, 2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 11
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>)  -- 12
   );

   Mountain_Clearings : constant Clearings_Arr := (
      (Rabbit, 2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 1
      (Rabbit, 2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 2
      (Rabbit, 2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 3
      (Mouse,  2, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 4
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 5
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 6
      (Mouse,  1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 7
      (Fox,    1, False,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 8
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 9
      (Rabbit, 1, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 10
      (Fox,    2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>), -- 11
      (Mouse,  2, True,
       Ruled_By | Warriors | Tokens | Buildings => <>)  -- 12
   );

end Root.Maps;

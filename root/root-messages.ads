-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                             ROOT . MESSAGES                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the protocol the server uses for communicating with    --
-- clients.                                                                  --
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
with Root.Faction; use Root.Faction;
with Root.Maps; use Root.Maps;

package Root.Messages is

   type Message_Type is (
      Game,
      Faction,
      Player
   ) with Size => 8;

   for Message_Type use (
      Game    => 0,
      Faction => 1,
      Player  => 2
   );

   -------------
   -- Faction --
   -------------
   type Msg_Faction (Faction : Faction_Type) is record
      Warrior_Supply : Integer;
      case Faction is
         when Duchy =>
            Burrow : Integer;
         when others =>
              null;
      end case;
   end record;

end Root.Messages;

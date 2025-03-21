-------------------------------------------------------------------------------
--                                                                           --
--                        THE ROOTBOTICS ASSISTANT                           --
--                                                                           --
--                               GAME (Body)                                 --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the terminal client for managing Leder Games' Root:    --
-- Clockwork Expansion factions.                                             --
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
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

with Client;
with Root; use Root;

package body Game is

   procedure Receive (Stream   : not null access Root_Stream_Type'Class;
                      Msg_Type : Message_Type) is
   begin
      case Msg_Type is
         when others =>
            Put_Line ("> ERROR: We shouldn't be here!");
            --  TODO: Exit with error
      end case;
   end Receive;

   procedure Create_Game is
      Payload : constant Create_Game_Msg := (
         AdSet        => False,
         Deck         => Exiles_And_Partisans,
         Map          => Fall,
         Clearing_Set => Balanced,
         Padding      => False,
         Num_Players  => 4
      );
   begin
      Client.Send (Payload);
   end Create_Game;

   procedure Map_Clears is
      Payload : constant Map_Clears_Msg := (
         Clearing_Suits => [1 .. 4  => Fox,
                            5 .. 8  => Mouse,
                            9 .. 12 => Rabbit]
      );
   begin
      Client.Send (Payload);
   end Map_Clears;

end Game;

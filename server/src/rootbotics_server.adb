-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                            ROOTBOTICS SERVER                              --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the main server loop for managing Leder Games' Root:   --
-- Clockwork Expansion bots.                                                 --
--                                                                           --
-- The Root Faction Assistant is free software: you can redistribute it      --
-- and/or modify it under the terms of the GNU General Public License as     --
-- published by the Free Software Foundation, either version 3 of the        --
-- License, or (at your option) any later version.                           --
--                                                                           --
-- The Root Faction Assistant is distributed in the hope that it will be     --
-- useful, but WITHOUT ANY WARRANTY; wihtout even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General  --
-- Public License for more details.                                          --
--                                                                           --
-- You should have received a copy of the GNU General Public License along   --
-- with The Rootbotics Assistant. If not, see                                --
-- <https://www.gnu.org/licenses/>.                                          --
-------------------------------------------------------------------------------
with Games; use Games;
with Messages; use Messages;
with Server;

procedure Rootbotics_Server is
   VERSION : constant String := "v0.3-dev";

   Curr_Game : Game_Access := null;
begin

   Server.Initialize;

   -- Waiting for game creation parameters --
   loop
      Server.Send (Request_Create_Game);
      Server.Receive (Create_Game);
      Curr_Game := Get_Current_Game;
      exit when Curr_Game /= null;
   end loop;

   loop
      Server.Send (Request_Map_Clears);
      Server.Receive (Map_Clears);
      exit when Curr_Game.Map_Clears_Set;
   end loop;

   Server.Finalize;

end Rootbotics_Server;

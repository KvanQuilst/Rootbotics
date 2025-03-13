-------------------------------------------------------------------------------
--                                                                           --
--                          ROOT FACTION ASSISTANT                           --
--                                                                           --
--                             FACTIONS (Spec)                               --
--                                                                           --
--                      Copyright (C) 2025 Dylan Eskew                       --
--                                                                           --
-- This file contains the specification of the common faction-related        --
-- subroutines used throughout The Rootbotics Assistant.                     --
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
with Ada.Streams;

with Root; use Root;
with Server;
with Types; use Types;

package Factions is

   type Faction_By_Seat is array (Seat) of Faction_Type;

   procedure Receive (
      Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Length : UInt8
   );

   -------------------
   -- Faction Class --
   -------------------
   type Faction (<>) is abstract new Server.Serializable with private;
   type Faction_Class is access Faction'Class;

   -- Constructor --
   function New_Faction (F_Type    : Faction_Type;
                         Clockwork : Boolean) return Faction_Class;

   -- Faction Concrete Methods --
   function Get_Faction (Self : Faction) return Faction_Type;

   function Score_Points (Self       : in out Faction;
                          Num_Points :        UInt8) return Boolean;

   procedure Add_Item    (Self : in out Faction;
                          I    :        Item);
   function  Remove_Item (Self : in out Faction;
                          I    :        Item) return Boolean;

   -- Faction Abstract Methods --
   procedure Setup     (Self : in out Faction) is abstract;
   procedure Take_Turn (Self : in out Faction) is abstract;

private

   -------------------
   -- Faction Class --
   -------------------
   type Faction (F_Type : Faction_Type) is
      abstract new Server.Serializable with
      record
         S      : Seat;
         Points : Point     := 0;
         Items  : Inventory := [others => 0];
      end record;

end Factions;

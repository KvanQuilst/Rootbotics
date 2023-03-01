with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Color; use Root.Color;
with Root.Map; use Root.Map;

with Root.Eyrie;
with Root.Marquise;
with Root.Alliance;
with Root.Vagabot; use Root.Vagabot;

procedure Rootbotics is
  VERSION : constant String := "v0.1";

  -- In order of setup priority --
  type Faction is (Marquise, Eyrie, Alliance, Vagabot);
                   --Lizards, Riverfolk, Corvids, Duchy);
  Playing : array (Faction'Range) of Boolean := (others => False);

  Map : Map_T;

begin
  Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
  New_Line;

  -----------------------
  -- Faction Selection --
  -----------------------
  Put_Line ("Which of the following clockwork factions will you be playing with:");
  Separator;
  Put      (" a. ");
  Root.Marquise.Put_Name (True);
  Put      (" b. ");
  Root.Eyrie.Put_Name (True);
  Put      (" c. ");
  Root.Alliance.Put_Name (True);
  Put      (" d. ");
  Root.Vagabot.Put_Name (True);
  Put_Line (" e. Logical Lizards  (unimplemented)");
  Put_Line (" f. Riverfolk Robots (unimplemented)");
  Put_Line (" g. Cogwheel Corvids (unimplemented)");
  Put_Line (" h. Drillbit Duchy   (unimplemented)");
  New_Line;
  Put_Line ("    No specified options to quit");
  Separator;

  -- Handle faction input --
  declare
    OL  : Option_List (1..Faction'Pos (Faction'Last));
  begin
    Get_List (OL, OL'Length);

    if OL (1) = Character'Val (0) then
      return;
    end if;

    for I in OL'Range loop
      if OL (I) /= Character'Val (0) then
        Playing (Faction'Val (Character'Pos (OL (I)) - 97)) := True;
      end if;
    end loop;
  end;
  New_Line;

  -------------------
  -- Map Selection --
  -------------------
  Put_Line ("Which map will you be playing on:");
  Separator;

  Put      (" a. ");
  Set_Color (Green);
  Put_Line ("Fall");
  Reset_Style;

  Put      (" b. ");
  Set_Color (Teal);
  Put_Line ("Winter   (unimplemented)");
  Reset_Style;

  Put      (" c. ");
  Set_Color (Blue);
  Put_Line ("Lake     (unimplemented)");
  Reset_Style;

  Put      (" d. ");
  Set_Color (Orange);
  Put_Line ("Mountain (unimplemented)");
  Reset_Style;

  Separator;

  -- Handle map input --
  declare
    Option : Character;
  begin
    -- TODO Implement other maps --
    Get_Option (Option, 1);

    case Option is
      when 'a' => Map := Fall_Map;
      --when 'b' => Map := Winter_Map;
      --when 'c' => Map := Lake_Map
      --when 'd' => Map := Mountain_Map;
      when others => 
        Put_Line ("ERROR: Should never reach here!");
        return;
    end case;
  end;
  New_Line;
  
  ----------------------------
  -- Faction Setup in Order --
  ----------------------------
  for I in Playing'Range loop
    if Playing (I) then
      case I is
        when Marquise => Root.Marquise.Setup (Map);
        when Eyrie => Root.Eyrie.Setup;
        when Alliance => Put_Line ("The Automated Alliance is unimplmented!");
        when Vagabot => Root.Vagabot.Setup;
        --TODO Clockwork Expansion 2
        --when Lizards => Put_Line ("The Logical Lizards are unimplemented!");
        --when Riverfolk => Put_Line ("The Riverfolk Robots are unimplemented!");
        --when Corvids => Put_Line ("The Cogwheel Corvids are unimplemented!");
        --when Duchy => Put_Line ("The Drillbit Duchy is unimplemented!");
      end case;
    end if;
  end loop;

  ---------------------
  -- Manage the Game --
  ---------------------
  loop
    Root.Eyrie.Take_Turn (Rabbit, Root.Map.Fall_Map);
  end loop;
end Rootbotics;

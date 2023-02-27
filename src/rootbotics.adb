with Ada.Text_IO; use Ada.Text_IO;

with Root; use Root;
with Root.Map;

with Root.Eyrie;
with Root.Marquise;
with Root.Alliance;
with Root.Vagabot; use Root.Vagabot;

procedure Rootbotics is
  VERSION : constant String := "v0.1";

  -- In order of setup priority --
  type Faction is (Marquise, Eyrie, Alliance, Vagabot, 
                   Lizards, Riverfolk, Corvids, Duchy);

  OL  : Option_List (1..Faction'Pos (Faction'Last));

  Playing : array (Faction'Range) of Boolean := (others => False);
begin
  Put_Line ("Welcome to the Rootbotics Logic Tool " & VERSION & "!");
  New_Line;
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
  Put_Line (" e. Logical Lizards");
  Put_Line (" f. Riverfolk Robots");
  Put_Line (" g. Cogwheel Corvids");
  Put_Line (" h. Drillbit Duchy");
  New_Line;
  Put_Line ("    No specified options to quit");
  Separator;

  Get_List (OL, OL'Length);

  if OL (1) = Character'Val (0) then
    return;
  end if;

  for I in OL'Range loop
    if OL (I) /= Character'Val (0) then
      Playing (Faction'Val (Character'Pos (OL (I)) - 97)) := True;
    end if;
  end loop;

  -- Faction Setup in Order --
  for I in Playing'Range loop
    if Playing (I) then
      case I is
        when Marquise => Root.Marquise.Setup (Root.Map.Fall_Map);
        when Eyrie => Root.Eyrie.Setup;
        when Alliance => Put_Line ("The Automated Alliance is unimplmented!");
        when Vagabot => Root.Vagabot.Setup;
        when Lizards => Put_Line ("The Logical Lizards are unimplemented!");
        when Riverfolk => Put_Line ("The Riverfolk Robots are unimplemented!");
        when Corvids => Put_Line ("The Cogwheel Corvids are unimplemented!");
        when Duchy => Put_Line ("The Drillbit Duchy is unimplemented!");
      end case;
    end if;
  end loop;

  loop
    Root.Eyrie.Take_Turn (Rabbit, Root.Map.Fall_Map);
  end loop;
end Rootbotics;
